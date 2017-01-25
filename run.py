from vunit import VUnit
from os.path import join, dirname

prj = VUnit.from_argv()
root = dirname(__file__)

udp_ip_lib = prj.add_library('miniblaze')
udp_ip_lib.add_source_files(join(root, 'src', 'hw1', 'generic_hdl_fifo.vhd'))
udp_ip_lib.add_source_files(join(root, 'test', 'Simu', 'tb_generic_hdl_fifo.vhd'))

prj.main()
