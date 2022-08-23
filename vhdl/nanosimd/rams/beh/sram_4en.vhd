----------------------------------------------------------------------------------------------
--
--      Input file         : sram_4en.vhd
--      Design name        : sram_4en
--      Author             : Tamar Kranenburg
--      Company            : Delft University of Technology
--                         : Faculty EEMCS, Department ME&CE
--                         : Systems and Circuits group
--
--      Description          : Single Port Synchronous Random Access Memory with 4 write enable
--                             ports.
--      Architecture 'arch'  : Default implementation
--      Architecture 'arch2' : Alternative implementation
--
----------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--library std;
use std.textio.all;

-- pragma translate_off
library work;
use work.func_pkg.notx;
-- pragma translate_on

entity sram_4en is 
  generic (
    file_name_g  : string   := "none"; 
    data_width_g : positive := 32;
    addr_width_g : positive := 16
  );
  port (
    clk_i  : in  std_ulogic;
    we_i   : in  std_ulogic_vector(data_width_g/8-1 downto 0);   
    en_i   : in  std_ulogic;
    addr_i : in  std_ulogic_vector(addr_width_g-1 downto 0);
    di_i   : in  std_ulogic_vector(data_width_g-1 downto 0);
    do_o   : out std_ulogic_vector(data_width_g-1 downto 0)
  );
end sram_4en;

-- Although this memory is very easy to use in conjunction with Modelsims mem load, it is not
-- supported by many devices (although it comes straight from the library. Many devices give
-- cryptic synthesization errors on this implementation, so it is not the default.
architecture beh of sram_4en is
  constant entries : integer := (2**addr_width_g);

  type ram_t is array(entries-1 downto 0) of std_ulogic_vector(data_width_g-1 downto 0);
  type sel_t is array(data_width_g/8-1 downto 0) of std_ulogic_vector(7 downto 0);

  shared variable ram : ram_t := (others => (others => '0'));
  signal di  : sel_t;

  -------------------------------------------------------------------------------
  -- mem_init
  -------------------------------------------------------------------------------
  procedure mem_init(variable RAM : inout ram_t) is
    file     readfile       : text open read_mode is file_name_g;
    variable vecline        : line;

    variable add_ulv        : std_ulogic_vector(addr_width_g-1 downto 0);
    variable var_ulv        : std_ulogic_vector(data_width_g-1 downto 0);

    variable add_int        : natural   := 0;
    variable addr_indicator : character := ' ';
  begin
    readline(readfile, vecline);

    read(vecline, addr_indicator);
    assert addr_indicator = '@' report "Incorrect address indicator: expecting @address" severity failure;

    hread(vecline, add_ulv);
    add_int := to_integer(unsigned(add_ulv));
        	      
    while not endfile(readfile) loop
      ----------------------------------------------------------------------
      -- read data
      -----------------------------------------------------------------------
      readline(readfile, vecline);
      hread(vecline, var_ulv);

      if(add_int > entries-1) then
        assert false report "Address out of range while filling memory" severity failure;
      else
        ram(add_int) := var_ulv;
        add_int      := add_int + 1;
      end if;
    end loop;
    assert false report "Memory filling successfull" severity note;
  end procedure mem_init;

  
begin
  process(we_i, di_i, addr_i)
  begin
    for i in 0 to data_width_g/8 - 1 loop
      if we_i(i) = '1' then
        di(i) <= di_i((i+1)*8 - 1 downto i*8);
      else
-- pragma translate_off      
        if(notx(addr_i)) then
-- pragma translate_on        
          di(i) <= ram(to_integer(unsigned(addr_i)))((i+1)*8-1 downto i*8);
-- pragma translate_off          
        else
          di(i) <= (others=>'X'); -- let undefined state propagate without complaning about
        end if;
-- pragma translate_on
      end if;
    end loop;
  end process;

  mem: process(clk_i)
    --variable ram          : ram_t;
    variable readcontents : boolean := true;
  begin

    if(readcontents = true and file_name_g /= "none") then
      mem_init(ram);
      readcontents := false;
    end if;
	  
    if rising_edge(clk_i) then
      if en_i = '1' then
        ram(to_integer(unsigned(addr_i))) := di(3) & di(2) & di(1) & di(0);
        do_o                              <= di(3) & di(2) & di(1) & di(0);
      end if;
    end if;
  end process mem;
end architecture beh;

