import Chisel._

class SdramController(
  // MHz
  val freqency: Double,
  // us
  val tPowerUp: Double,
  // ns
  val cRP: Int,
  val cRC: Int,
  val cMRD: Int
) extends Module {

  val cPowerUp = tPowerUp * frequency * conv_us toInt

  class SdramInterface extends Bundle {
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
  }

  val io = new Bundle {
    val cmd = Decoupled (new Bundle { 
      val we = Bool (INPUT);
      val addr = UInt (width = 22);
    });
    val wdata = Valid(UInt(width = 16)).flip;
    val rdata = Valid(UInt(width = 16));
    val sdram = new SdramInterface();
  }

  val POWERON :: IDLE :: AUTO_REFRESH :: WRITEA :: READA :: Nil = Enum(UInt(), 5);

  val state = RegInit(POWERON)
  val state_counter = RegNext(state_counter + 1, Uint(0))
  val refresh_counter = RegNext(refresh_counter + 1, UInt(0))

  io.sdram.cke := Bool(True)

  def changeState(s: UInt) {
    state := s
    state_counter := UInt(0)
  }

  // commands
  def issueNop {
    io.sdram.xcs   := Bool(False)
    io.sdram.xras  := Bool(True)
    io.sdram.xcas  := Bool(True)
    io.sdram.xwe   := Bool(True)
  }

  def issuePrechargeAll {
    io.sdram.xcs   := Bool(False)
    io.sdram.xras  := Bool(False)
    io.sdram.xcas  := Bool(True)
    io.sdram.xwe   := Bool(False)
    io.sdram.a(10) := Bool(True)
  }

  def issueRefresh {
    io.sdram.xcs  := Bool(False)
    io.sdram.xras := Bool(False)
    io.sdram.xcas := Bool(False)
    io.sdram.xwe  := Bool(True)
  }

  def issueModeRegisterSet(data: Short) {
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

  switch(state) {
    is (POWERON) {
      io.cmd.ready   := Bool(False)
      io.rdata.valid := Bool(False)

      when (state_counter < cPowerOn) {
        issueNop

      } .elsewhen (state_counter === UInt(cPowerOn)) {
        issuePrechargeAll

      } .elsewhen (state_counter <   UInt(cPowerOn + cRP)) {
        issueNop

      } .elsewhen (state_counter === UInt(cPowerOn + cRP)) {
        issueRefresh

      } .elsewhen (state_counter <   UInt(cPower + cRP + cRC)) {
        issueNop

      } .elsewhen (state_counter === UInt(cPower + cRP + cRC)) {
        issueRefresh

      } .elsewhen (state_counter <   UInt(cPower + cRP + 2 * cRC)) {
        issueNop

      } .elsewhen (state_counter === UInt(cPower + cRP + 2 * cRC)) {
        // CAS Latency is 3
        // Burst Type is interleaved
        // Burst Length is 8
        issueModeRegisterSet(0x3b) // 0_00_011_1_011

      } .elsewhen (state_counter <   UInt(cPower + cRP + 2 * cRC + cMRD)) {
        issueNop

      } .otherwise {
        issueNop
        changeState(IDLE)
      }
    }
  } is (IDLE) {
    // FIXME
  } is (WRITEA) {
    // FIXME
  } is (READA) {
    // FIXME
  } is (AUTO_REFRESH) {
    // FIXME
  }
}

object SdramController {

  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new Sdram)) { c =>
      new SdramTests(c)
    }
  }

  class SdramControllerTests(c: SdramController) extends Tester(c, isTrace = false) {
    // FIXME
  }
}
