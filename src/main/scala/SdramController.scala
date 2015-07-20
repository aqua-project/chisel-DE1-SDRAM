import Chisel._

class SdramController() extends Module {
  class SdramInterface extends Bundle {
    val a    = Bits(OUTPUT, 13);
    val ba   = Bits(OUTPUT, 2);
    val xcas = Bool(OUTPUT);
    val cke  = Bool(OUTPUT);
    val xcs  = Bool(OUTPUT);
    val dqml = Bool(OUTPUT);
    val dqmh = Bool(OUTPUT);
    val dqi  = Bits(INPUT, 16);
    val dqo  = Bits(OUTPUT, 16);
    val xras = Bool(OUTPUT);
    val xwe  = Bool(OUTPUT);
  }

  val io = new Bundle {
    val enq = Decoupled(UInt(width = 128)).flip;
    val deq = Decoupled(UInt(width = 128));
    val sdram = new SdramInterface();
  }

  val idle :: refresh :: write :: read :: Nil = Enum(UInt(), 4);

  // FIXME
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
