# 2-way-Superscalar-vhdl
Responsable for instruction scheduling by the hardware.
reorder_buffer can solve the name dependence (WAW and WAR) and achieve the register renaming.
register alias table can detect the data dependence (RAW) and register renaming if data-dependent.
reservation stations temporarily store the instructions. If both operands ready send prepared instructions for execution.
