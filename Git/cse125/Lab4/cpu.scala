// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.6 of Patterson and Hennessy
 * This follows figure 4.49
 */
class PipelinedCPU(implicit val conf: CPUConfig) extends Module {
  val io = IO(new CoreIO)

  // Bundles defining the pipeline registers and control structures

  // Everything in the register between IF and ID stages
  class IFIDBundle extends Bundle {
    val instruction = UInt(32.W)
    val pc          = UInt(32.W)
    val pcplusfour  = UInt(32.W)
  }

  // Control signals used in EX stage
  class EXControl extends Bundle {
    val add       = Bool()
    val immediate = Bool()
    val alusrc1   = UInt(2.W)
    val branch    = Bool()
    val jump      = UInt(2.W)
  }

  // Control signals used in MEM stage
  class MControl extends Bundle {
	  //mycode
	  val memread = Bool()
    val memwrite = Bool()
    val maskmode = UInt(2.W)
    val sext = Bool()
    val taken = Bool()
  }

  // Control signals used in EB stage
  class WBControl extends Bundle {
    //mycode
    val toreg = UInt(2.W)
    val regwrite = Bool()
  }

  //mycode
  class IDEXControl extends Bundle {
    val pc = UInt(32.W)
    val pcplusfour = UInt(32.W)
    val sextimm = UInt(32.W)
    val readdata1 = UInt(32.W)
    val readdata2 = UInt(32.W)
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val funct7 = UInt(7.W)
    val funct3 = UInt(3.W)
    val writereg = UInt(5.W)
  }

  // Everything in the register between ID and EX stages
  class IDEXBundle extends Bundle {

    val excontrol = new EXControl
    val mcontrol  = new MControl
    val wbcontrol = new WBControl
  }

  // Everything in the register between ID and EX stages
  class EXMEMBundle extends Bundle {

    val mcontrol  = new MControl
    val wbcontrol = new WBControl
  }

  //mycode
  class EXMEMControl extends Bundle {
    val pcplusfour = UInt(32.W)
    val aluresult = UInt(32.W)
    val pcnext = UInt(32.W)
    val readdata2 = UInt(32.W)
    val writereg = UInt(5.W)
  }

  class MEMWBControl extends Bundle {
    val pcplusfour = UInt(32.W)
    val aluresult = UInt(32.W)
    val readdata = UInt(32.W)
    val writereg = UInt(5.W)
  }
  // Everything in the register between ID and EX stages
  class MEMWBBundle extends Bundle {

    val wbcontrol = new WBControl
  }

  // All of the structures required
  val pc         = RegInit(0.U)
  val control    = Module(new Control())
  val branchCtrl = Module(new BranchControl())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val pcPlusFour = Module(new Adder())
  val branchAdd  = Module(new Adder())
  val forwarding = Module(new ForwardingUnit())  //pipelined only
  val hazard     = Module(new HazardUnit())      //pipelined only
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  val if_id      = RegInit(0.U.asTypeOf(new IFIDBundle))
  val id_ex      = RegInit(0.U.asTypeOf(new IDEXBundle))
  val ex_mem     = RegInit(0.U.asTypeOf(new EXMEMBundle))
  val mem_wb     = RegInit(0.U.asTypeOf(new MEMWBBundle))
  //mycode
  val id_ex_control = RegInit(0.U.asTypeOf(new IDEXControl))
  val ex_mem_control = RegInit(0.U.asTypeOf(new EXMEMControl))
  val mem_wb_control = RegInit(0.U.asTypeOf(new MEMWBControl))

  /*
  // Remove these as you hook up each one
  control.io    := DontCare
  branchCtrl.io := DontCare
  registers.io := DontCare
  aluControl.io := DontCare
  alu.io := DontCare
  immGen.io := DontCare
  pcPlusFour.io := DontCare
  branchAdd.io := DontCare
  io.dmem := DontCare*/
  //forwarding.io := DontCare
  //hazard.io := DontCare

  printf("Cycle=%d ", cycleCount)

  // Forward declaration of wires that connect different stages

  // From memory back to fetch. Since we don't decide whether to take a branch or not until the memory stage.
  val next_pc = Wire(UInt(32.W))
  //next_pc    := DontCare    // remove when connected

  // For wb back to other stages
  val write_data = Wire(UInt(32.W))
  //write_data    := DontCare // remove when connected

  /////////////////////////////////////////////////////////////////////////////
  // FETCH STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Note: This comes from the memory stage!
  // Only update the pc if the pcwrite flag is enabled
  //pc := pcPlusFour.io.result
  pc := MuxCase(0.U, Array(
    (hazard.io.pcwrite === 0.U) -> pcPlusFour.io.result,
    (hazard.io.pcwrite === 1.U) -> next_pc,
    (hazard.io.pcwrite === 2.U) -> pc
  ))

  // Send the PC to the instruction memory port to get the instruction
  io.imem.address := pc

  // Get the PC + 4
  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  // Fill the IF/ID register if we are not bubbling IF/ID
  // otherwise, leave the IF/ID register *unchanged*
  if_id.instruction := io.imem.instruction
  if_id.pc          := pc
  if_id.pcplusfour  := pcPlusFour.io.result
  //!
  //if_id.flush := hazard.io.ifid_flush
  printf(p"IF/ID: $if_id\n")

  /////////////////////////////////////////////////////////////////////////////
  // ID STAGE
  /////////////////////////////////////////////////////////////////////////////

  val rs1 = if_id.instruction(19,15)
  val rs2 = if_id.instruction(24,20)

  // Send input from this stage to hazard detection unit
  hazard.io.rs1 := rs1
  hazard.io.rs2 := rs2
  // Send opcode to control
  control.io.opcode := if_id.instruction(6,0)

  // Send register numbers to the register file
  registers.io.readreg1 := rs1
  registers.io.readreg2 := rs2

  // Send the instruction to the immediate generator
  immGen.io.instruction := if_id.instruction

  // FIll the id_ex register
  id_ex_control.pc := if_id.pc
  id_ex_control.pcplusfour := if_id.pcplusfour
  id_ex_control.sextimm := immGen.io.sextImm
  id_ex_control.readdata1 := registers.io.readdata1
  id_ex_control.readdata2 := registers.io.readdata2
  id_ex_control.rs1 := rs1
  id_ex_control.rs2 := rs2
  id_ex_control.funct7 := if_id.instruction(31,25)
  id_ex_control.funct3 := if_id.instruction(14,12)
  id_ex_control.writereg := if_id.instruction(11,7)

  // Set the execution control signals
  id_ex.excontrol.add := control.io.add
  id_ex.excontrol.immediate := control.io.immediate
  id_ex.excontrol.alusrc1 := control.io.alusrc1
  id_ex.excontrol.branch := control.io.branch
  id_ex.excontrol.jump := control.io.jump

  // Set the memory control signals
  id_ex.mcontrol.memread := control.io.memread
  id_ex.mcontrol.memwrite := control.io.memwrite
  id_ex.mcontrol.maskmode := if_id.instruction(13,12)
  id_ex.mcontrol.sext := ~if_id.instruction(14)
  id_ex.mcontrol.taken := false.B

  // Set the writeback control signals
  id_ex.wbcontrol.toreg := control.io.toreg
  id_ex.wbcontrol.regwrite := control.io.regwrite
  //!
  //id_ex.flush := hazard.io.idex_bubble
  printf("DASM(%x)\n", if_id.instruction)
  printf(p"ID/EX: $id_ex\n")

  /////////////////////////////////////////////////////////////////////////////
  // EX STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set the inputs to the hazard detection unit from this stage (SKIP FOR PART I)
  hazard.io.idex_memread := id_ex.mcontrol.memread
  hazard.io.idex_rd := id_ex_control.writereg
  // Set the input to the forwarding unit from this stage (SKIP FOR PART I)
  forwarding.io.rs1 := id_ex_control.rs1
  forwarding.io.rs2 := id_ex_control.rs2
  // Connect the ALU control wires (line 45 of single-cycle/cpu.scala)
  aluControl.io.add       := id_ex.excontrol.add
  aluControl.io.immediate := id_ex.excontrol.immediate
  aluControl.io.funct7    := id_ex_control.funct7
  aluControl.io.funct3    := id_ex_control.funct3
  // Insert the forward inputx mux here (SKIP FOR PART I)
  val forward_inputx = Wire(UInt(32.W))
  forward_inputx := MuxCase(0.U, Array(
    (forwarding.io.forwardA === 0.U) -> id_ex_control.readdata1,
    (forwarding.io.forwardA === 1.U) -> ex_mem_control.aluresult,
    (forwarding.io.forwardA === 2.U) -> write_data
  ))
  // Insert the ALU inpux mux here (line 59 of single-cycle/cpu.scala)
  val alu_inputx = Wire(UInt(32.W))
  val forward_inputy = Wire(UInt(32.W))
  /*switch(id_ex.excontrol.alusrc1) {
    is(0.U) { alu_inputx := forward_inputx }
    is(1.U) { alu_inputx := 0.U }
    is(2.U) { alu_inputx := id_ex_control.pc }
  }*/
  alu_inputx := MuxCase(0.U, Array(
    (id_ex.excontrol.alusrc1 === 0.U) -> forward_inputx,
    (id_ex.excontrol.alusrc1 === 1.U) -> 0.U,
    (id_ex.excontrol.alusrc1 === 2.U) -> id_ex_control.pc
  ))
  alu.io.inputx := alu_inputx
  // Insert forward inputy mux here (SKIP FOR PART I)
  forward_inputy := MuxCase(0.U, Array(
    (forwarding.io.forwardB === 0.U) -> id_ex_control.readdata2,
    (forwarding.io.forwardB === 1.U) -> ex_mem_control.aluresult,
    (forwarding.io.forwardB === 2.U) -> write_data
  ))
  // Input y mux (line 66 of single-cycle/cpu.scala)
  val alu_inputy = Wire(UInt(32.W))
  alu_inputy := forward_inputy
  alu.io.inputy := Mux(id_ex.excontrol.immediate, id_ex_control.sextimm, alu_inputy)
  // Connect the branch control wire (line 54 of single-cycle/cpu.scala)
  branchCtrl.io.branch := id_ex.excontrol.branch
  branchCtrl.io.funct3 := id_ex_control.funct3
  branchCtrl.io.inputx := forward_inputx
  branchCtrl.io.inputy := forward_inputy
  // Set the ALU operation
  alu.io.operation := aluControl.io.operation

  // Connect the branchAdd unit
  branchAdd.io.inputx := id_ex_control.pc
  branchAdd.io.inputy := id_ex_control.sextimm
  // Set the EX/MEM register values
  ex_mem_control.pcplusfour := id_ex_control.pcplusfour
  ex_mem_control.aluresult := alu.io.result
  ex_mem_control.readdata2 := alu_inputy
  ex_mem_control.writereg := id_ex_control.writereg

  ex_mem.mcontrol := id_ex.mcontrol
  ex_mem.wbcontrol := id_ex.wbcontrol
  // Calculate whether which PC we should use and set the taken flag (line 92 in single-cycle/cpu.scala)
  when (branchCtrl.io.taken || id_ex.excontrol.jump === 2.U) {
    ex_mem_control.pcnext := branchAdd.io.result
    ex_mem.mcontrol.taken := true.B
  } .elsewhen (id_ex.excontrol.jump === 3.U) {
    ex_mem_control.pcnext := alu.io.result & Cat(Fill(31, 1.U), 0.U)
    ex_mem.mcontrol.taken := true.B
  } .otherwise {
    ex_mem_control.pcnext := 0.U
    ex_mem.mcontrol.taken := false.B
  }

  //!
  //ex_mem.io.flush := hazard.io.exmem_bubble
  printf(p"EX/MEM: $ex_mem\n")

  /////////////////////////////////////////////////////////////////////////////
  // MEM STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set data memory IO (line 71 of single-cycle/cpu.scala)
  io.dmem.address   := ex_mem_control.aluresult
  io.dmem.writedata := ex_mem_control.readdata2
  io.dmem.memread   := ex_mem.mcontrol.memread
  io.dmem.memwrite  := ex_mem.mcontrol.memwrite
  io.dmem.maskmode  := ex_mem.mcontrol.maskmode
  io.dmem.sext      := ex_mem.mcontrol.sext
  // Send next_pc back to the fetch stage
  next_pc := ex_mem_control.pcnext
  // Send input signals to the hazard detection unit (SKIP FOR PART I)
  hazard.io.exmem_taken := ex_mem.mcontrol.taken
  // Send input signals to the forwarding unit (SKIP FOR PART I)
  forwarding.io.exmemrd := ex_mem_control.writereg
  forwarding.io.exmemrw := ex_mem.wbcontrol.regwrite
  // Wire the MEM/WB register
  mem_wb_control.pcplusfour := ex_mem_control.pcplusfour
  mem_wb_control.aluresult := ex_mem_control.aluresult
  mem_wb_control.readdata := io.dmem.readdata
  mem_wb_control.writereg := ex_mem_control.writereg

  mem_wb.wbcontrol := ex_mem.wbcontrol

  printf(p"MEM/WB: $mem_wb\n")

  /////////////////////////////////////////////////////////////////////////////
  // WB STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set the writeback data mux (line 78 single-cycle/cpu.scala)
  //val write_data = Wire(UInt())
  when (mem_wb.wbcontrol.toreg === 1.U) {
    write_data := mem_wb_control.readdata
  } .elsewhen (mem_wb.wbcontrol.toreg === 2.U) {
    write_data := mem_wb_control.pcplusfour
  } .otherwise {
    write_data := mem_wb_control.aluresult
  }
  // Write the data to the register file
  registers.io.writereg := mem_wb_control.writereg
  registers.io.wen := mem_wb.wbcontrol.regwrite && (mem_wb_control.writereg =/= 0.U)
  registers.io.writedata := write_data
  // Set the input signals for the forwarding unit (SKIP FOR PART I)
  forwarding.io.memwbrd := mem_wb_control.writereg
  forwarding.io.memwbrw := mem_wb.wbcontrol.regwrite
  printf("---------------------------------------------\n")
}
