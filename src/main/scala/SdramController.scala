import Chisel._

class SdramController (
  val freqency: Double, // MHz
  val casLatency: Int,
  val burstLength: Int,
  val interleave: Boolean,
  val tPowerUp: Double, //us
  val tRef: Double,     // ms
  val cRCD: Int,
  val cRAS: Int,
  val cDPL: Int,
  val cRP: Int,
  val cRC: Int,
  val cMRD: Int
) extends Module {

  val conv_ms: Double = 1000
  val conv_ns: Double = 1

  val cRef = tRef / 8400 * frequency * conv_ms toInt
  val cPowerUp = tPowerUp * frequency * conv_us toInt
  val refThres = 100 // FIXME

  val io = new Bundle {
    val cmd = Decoupled (new Bundle { 
      val we = Bool (INPUT);
      val addr = UInt (width = 25);
    });
    val wdata = Valid(UInt(width = 16)).flip;
    val rdata = Valid(UInt(width = 16));
    val sdram = new Bundle {
      val a    = Bits(OUTPUT, 13)
      val ba   = Bits(OUTPUT, 2)
      val xcas = Bool(OUTPUT)
      val cke  = Bool(OUTPUT)
      val xcs  = Bool(OUTPUT)
      val dqml = Bool(OUTPUT)
      val dqmh = Bool(OUTPUT)
      val dqi  = Bits(INPUT, 16)
      val dqo  = Bits(OUTPUT, 16)
      val xras = Bool(OUTPUT)
      val xwe  = Bool(OUTPUT)
    };
  }

  val readbuf  = new Pipe(UInt(width = 16))
  val writebuf = Pipe(io.wdata, tRCD + 2)

  readbuf.enq.bits  := UInt(0) // Default Value
  readbuf.enq.valid := Bool(False) // Default Value
  readbuf.deq       <> io.rdata

  val address = Reg(UInt(width = 25))
  val data = Reg(UInt(width = 16))

  val POWERON :: IDLE :: AUTO_REFRESH :: WRITEA :: READA :: Nil = Enum(UInt(), 5);
  val state = RegInit(POWERON)
  val state_counter = RegNext(state_counter + 1, Uint(0))

  io.sdram.cke  := Bool(True)
  io.sdram.dqml := Bool(True) // Default Value
  io.sdram.dqmh := Bool(True) // Default Value

  def changeState(s: UInt) {
    state := s
    state_counter := UInt(0)
  }

  // commands
  def issueNOP {
    io.sdram.xcs   := Bool(False)
    io.sdram.xras  := Bool(True)
    io.sdram.xcas  := Bool(True)
    io.sdram.xwe   := Bool(True)
  }

  def issuePAll {
    io.sdram.xcs   := Bool(False)
    io.sdram.xras  := Bool(False)
    io.sdram.xcas  := Bool(True)
    io.sdram.xwe   := Bool(False)
    io.sdram.a(10) := Bool(True)
  }

  val refresh_counter = RegNext(refresh_counter - SInt(1), SInt(cRef))
  def issueREF {
    io.sdram.xcs    := Bool(False)
    io.sdram.xras   := Bool(False)
    io.sdram.xcas   := Bool(False)
    io.sdram.xwe    := Bool(True)
    refresh_counter := refresh_counter + SInt(cRef)
  }

  def issueMRS(data: Short) {
    // data = A_BB_CCC_D_EEE
    // A: Write Burst Mode
    // B: Operating Mode
    // C: Latency Mode
    // D: Burst Type
    // E: Burst Length

    io.sdram.xcs       := Bool(False)
    io.sdram.xras      := Bool(False)
    io.sdram.xcas      := Bool(False)
    io.sdram.xwe       := Bool(False)
    io.sdram.ba        := Bits(0)
    io.sdram.a(12, 10) := Bits(0)
    io.sdram.a(9, 0)   := Bits(data , 10)
  }

  def issueACT(bank: UInt, rowaddr: UInt) {
    io.sdram.xcs     := Bool(False)
    io.sdram.xras    := Bool(False)
    io.sdram.xcas    := Bool(True)
    io.sdram.xwe     := Bool(Truee)
    io.sdram.ba      := bank
    io.sdram.a       := rowaddr
  }

  def issueREAD(bank: UInt, coladdr: UInt, autoPrecharge: Boolean = False) {
    io.sdram.xcs     := Bool(False)
    io.sdram.xras    := Bool(True)
    io.sdram.xcas    := Bool(False)
    io.sdram.xwe     := Bool(Truee)
    io.sdram.ba      := bank
    io.sdram.a(9, 0) := coladdr
    io.sdram.a(10)   := Bool(autoPrecharge)
  }

  def issueWRITE(coladdr: UInt, autoPrecharge: Boolean = False) {
    io.sdram.xcs     := Bool(False)
    io.sdram.xras    := Bool(True)
    io.sdram.xcas    := Bool(False)
    io.sdram.xwe     := Bool(Truee)
    io.sdram.ba      := bank
    io.sdram.a(9, 0) := coladdr
    io.sdram.a(10)   := Bool(autoPrecharge)
  }

  switch(state) {
    is (POWERON) {
      io.cmd.ready   := Bool(False)

      when (state_counter < cPowerUp) {
        issueNOP

      } .elsewhen (state_counter === UInt(cPowerUp)) {
        issuePALL

      } .elsewhen (state_counter <   UInt(cPowerUp + cRP)) {
        issueNOP

      } .elsewhen (state_counter === UInt(cPowerUp + cRP)) {
        issueREF

      } .elsewhen (state_counter <   UInt(cPowerUp + cRP + cRC)) {
        issueNOP

      } .elsewhen (state_counter === UInt(cPowerUp + cRP + cRC)) {
        issueREF

      } .elsewhen (state_counter <   UInt(cPowerUp + cRP + 2 * cRC)) {
        issueNOP

      } .elsewhen (state_counter === UInt(cPowerUp + cRP + 2 * cRC)) {
        // CAS Latency is 3
        // Burst Type is interleaved
        // Burst Length is 8
        issueMRS(0x3b) // 0_00_011_1_011

      } .elsewhen (state_counter <   UInt(cPowerUp + cRP + 2 * cRC + cMRD)) {
        issueNOP

      } .otherwise {
        issueNOP
        changeState(IDLE)
      }
    }
  } is (IDLE) {
    issueNOP

    // <= is corrrect, because ready indicates state of next clock
    io.cmd.ready   := (refresh_counter <= UInt(refThres))

    when (refresh_counter < UInt(refThres)) {
      changeState(AUTO_REFRESH)

    } .otherwise {
      when (io.cmd.valid) {
        address := io.cmd.bits.addr

        when (io.cmd.bits.we) {
          changeState(WRITEA)
        } .otherwise {
          changeState(READA)
        }
      }
    }

  } is (WRITEA) {
    io.cmd.ready   := Bool(False)

    when (state_counter === UInt(0)) {
      issueACT(address(24,23), address(22,13))
    } .elsewhen (state_counter === UInt(cRCD)) {
      issueREAD(address(24, 23), address(12, 0), autoPrecharge = True)
    } .otherwise {
      issueNOP
    }

    when (state_counter >= UInt(max(tRCD + burstLength + tDPL + tRP, tRC)) - 1) {
      changeState(IDLE)
    }

    io.sdram.dqo  := writebuf.deq.bits
    io.sdram.dqmh := ~ writebuf.deq.valid
    io.sdram.dqml := ~ writebuf.deq.valid

  } is (READA) {
    io.cmd.ready   := Bool(False)

    when (state_counter === UInt(0)) {
      issueACT(address(24,23), address(24,13))
    } .elsewhen (state_counter === UInt(cRCD)) {
      issueREAD(address(24,23), address(12, 0), autoPrecharge = True)
    } .otherwise {
      issueNOP
    }

    when (state_counter >= UInt(tRCD) &&
          state_counter < UInt(tRCD + burstLength)) {
      io.sdram.dqml := Bool(False)
      io.sdram.dqmh := Bool(False)
    }

    when (state_counter >= UInt(tRCD + casLatenccy) &&
          state_counter < UInt(tRCD + burstLength + casLatency)) {
      readbuf.enq.bits  := io.sdram.dqi
      readbuf.enq.valid := Bool(True)
    }

    when (state_counter >= UInt(max(tRCD + casLatency + burstLength, tRC) - 1)) {
      changeState(IDLE)
    }

  } is (AUTO_REFRESH) {
    io.cmd.ready   := Bool(False)
    io.rdata.valid := Bool(False)

    when (state_counter === UInt(0)) {
      issueRefresh
    } .otherwise {
      issueNop
    }

    when (state_counter >= UInt(cRC)) {
      changeState(IDLE)
    }
  }
}

object SdramController {

  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new Sdram)) { c =>
      new SdramTests(c)
    }
  }

  class SdramControllerTests(c: SdramController) extends Tester(c, isTrace = false) {
    // FIXME
  }
}
