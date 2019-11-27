/*
 * Written by Oguz Meteer (info@guztech.nl)
 *
 * A project running on the Lattice iCEstick that uses
 * the Digilent PmodI2S and PmodENC boards. A sine wave
 * is generated and sent to the I2S board, and the pitch
 * is controlled using the Quadrature Encoder board.
 *
 * Pmod connections:
 *
 * ----------------------------------\
 * |   1-7                            \
 * | P 2-8      L3     /----\          -----|
 * | M 3-9   L2 L5 L4  |FPGA|          |USB |
 * | O 4-10     L1     |HX1K|          |    |
 * | D 5-11            \----/          -----|
 * |   6-12                           /
 * ----------------------------------/
 *
 * Pmod pin:
 * 1  = i2s_mclk  |
 * 2  = i2s_lrclk | PmodI2S
 * 3  = i2s_sclk  | board
 * 4  = i2s_sd    |
 *
 * 7  = enc_a     |
 * 8  = enc_b     | PmodENC
 * 9  = enc_btn   | board
 * 10 = enc_swt   |
 */

import spinal.core._
import spinal.lib._

class PLL100 extends BlackBox {
  val io = new Bundle {
    val clock_in  = in  Bool
    val clock_out = out Bool
    val locked    = out Bool
  }

  noIoPrefix()
}

class i2s_output extends Component {
  val io = new Bundle {
    val data_l    = in  SInt(16 bits)
    val data_r    = in  SInt(16 bits)
    val accepted  = out Bool
    val i2s_sd    = out Bool
    val i2s_lrclk = out Bool
    val i2s_sclk  = out Bool
    val i2s_mclk  = out Bool
  }

  // Define the registers
  val advance   = Reg(Bool)          init(False)
  val divider   = Reg(UInt(5 bits))  init(0)
  val step      = Reg(UInt(6 bits))  init(0)
  val shift_out = Reg(Bits(17 bits)) init(0)
  val hold_r    = Reg(Bits(16 bits)) init(0)
  val acc       = Reg(Bool)          init(False)

  // Assign registers to outputs
  io.i2s_lrclk := step.msb
  io.i2s_mclk  := divider(1)
  io.i2s_sclk  := step.lsb
  io.i2s_sd    := shift_out.msb
  io.accepted  := acc

  acc := False

  when(advance === True) {
    when(step(0) === True) {
      shift_out := shift_out(15 downto 0) ## 1

      when(step(5 downto 1).asBits === B"01111") {
        shift_out(15 downto 0) := hold_r
      }.elsewhen(step(5 downto 1).asBits === B"11111") {
        shift_out(15 downto 0) := io.data_l.asBits
        hold_r                 := io.data_r.asBits
        acc                    := True
      }
    }
    step := step + 1
  }

  when(divider === 0) {
    advance := True
  }.otherwise {
    advance := False
  }

  divider := divider + 1
}

/*
 * Outputs samples of a sine wave with 'sampleRes' resolution
 * and 'numSamples' samples. A new sample is placed on the
 * 'sin' output when 'rqst' is high. 'sampleStepSize' determines
 * the number of samples that should be skipped. A value of 1 takes
 * the next sample, a value of 2 outputs every second value, etc.
 */
class SinGen(sampleRes: Int, numSamples: Int, stepWidth: Int) extends Component {
  val io = new Bundle {
    val rqst           = in  Bool
    val sampleStepSize = in  UInt(stepWidth bits)
    val sin            = out SInt(sampleRes bits)
  }

  def sinTable = for (sampleIndex <- 0 until numSamples) yield {
    val sinValue = Math.sin(2 * Math.PI * sampleIndex / numSamples)
    S((sinValue * ((1 << sampleRes) / 2 - 1)).toInt, sampleRes bits)
  }

  val rom = Mem(SInt(sampleRes bits), initialContent = sinTable)
  val phase = Reg(UInt(log2Up(numSamples) bits)) init(0)

  when(io.rqst === True) {
    phase := phase + io.sampleStepSize
  }

  io.sin := rom.readSync(phase)
}

class QuadEnc(posSize: Int) extends Component {
  val io = new Bundle {
    val A   = in  Bool
    val B   = in  Bool
    val pos = out UInt(posSize bits)
  }

  val A_delayed = Reg(Bits(3 bits))       init(0)
  val B_delayed = Reg(Bits(3 bits))       init(0)
  val pos       = Reg(UInt(posSize bits)) init(0)

  val enable = Bool
  val dir    = Bool

  A_delayed := A_delayed(1 downto 0) ## io.A
  B_delayed := B_delayed(1 downto 0) ## io.B

  enable := A_delayed(1) ^ A_delayed(2) ^ B_delayed(1) ^ B_delayed(2)
  dir    := A_delayed(1) ^ B_delayed(2)

  when(enable === True) {
    when(dir === True) {
      pos := pos + 1
    }.otherwise {
      pos := pos - 1
    }
  }

  io.pos <> pos
}

/*
 * Top level component.
 *
 * io.pmod1(0 to 3) is connected to Pmod 0-3 and
 * io.pmod2(0 to 3) is connected to Pmod 7-10.
 */
class i2s_top extends Component {
  val io = new Bundle {
    val resetn   = in  Bool
    val clk12MHz = in  Bool
    val pmod2    = in  Bits(4 bits)
    val pmod1    = out Bits(4 bits)
    val leds     = out Bits(5 bits)
    val rqst     = out Bool
  }.setName("")

  // Create an Area to manage all clocks and reset
  val clkCtrl = new Area {
    // Instantiate and drive the PLL
    val pll = new PLL100
    pll.io.clock_in := io.clk12MHz

    // Create a new clock domain named "core"
    val coreClockDomain = ClockDomain.internal(
      name = "core",
      frequency = FixedFrequency(96 MHz)
    )

    // Drive clock and reset signals of the coreClockDomain
    coreClockDomain.clock := pll.io.clock_out
    coreClockDomain.reset := ResetCtrl.asyncAssertSyncDeassert(
      input = !io.resetn || !pll.io.locked,
      clockDomain = coreClockDomain
    )
  }

  // Create a ClockingArea which will be under the effect of the clkCtrl.coreClockDomain
  val core = new ClockingArea(clkCtrl.coreClockDomain) {
    val i2s = new i2s_output

    io.pmod1(0) <> i2s.io.i2s_mclk
    io.pmod1(1) <> i2s.io.i2s_lrclk
    io.pmod1(2) <> i2s.io.i2s_sclk
    io.pmod1(3) <> i2s.io.i2s_sd
    io.rqst     <> i2s.io.accepted

    val enc = new QuadEnc(8)

    enc.io.A   <> io.pmod2(0)
    enc.io.B   <> io.pmod2(1)

    val sinGen = new SinGen(sampleRes = 16, numSamples = 256, stepWidth = 6)

    i2s.io.data_l            <> sinGen.io.sin
    i2s.io.data_r            <> sinGen.io.sin
    sinGen.io.rqst           <> i2s.io.accepted
    sinGen.io.sampleStepSize <> enc.io.pos(7 downto 2)

    io.leds <> enc.io.pos.asBits(2 to 6)
  }
}

object i2s_top {
  val globalClockConfig = new ClockDomainConfig(
    clockEdge = RISING,
    resetKind = SYNC,
    resetActiveLevel = LOW
  )

  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "verilog",
      defaultConfigForClockDomains = globalClockConfig
    ).generate(new i2s_top).printPruned()
  }
}
