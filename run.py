from vunit import VUnit
from os.path import join, dirname

prj = VUnit.from_argv()
prj.enable_location_preprocessing()
root = dirname(__file__)

miniblaze_lib = prj.add_library('miniblaze')
miniblaze_lib.add_source_files(join(root, 'src', 'hw1', 'generic_hdl_fifo.vhd'))
miniblaze_lib.add_source_files(join(root, 'src', 'hw1', 'ALU.vhd'))
miniblaze_lib.add_source_files(join(root, 'src', 'hw1', 'ALU_pkg.vhd'))
miniblaze_lib.add_source_files(join(root, 'test', 'Simu', 'tb_generic_hdl_fifo.vhd'))
miniblaze_lib.add_source_files(join(root, 'test', 'Simu', 'tb_ALU.vhd'))
miniblaze_lib.add_source_files(join(root, 'test', 'Simu', 'data_pkg.vhd'))

prj.main()
