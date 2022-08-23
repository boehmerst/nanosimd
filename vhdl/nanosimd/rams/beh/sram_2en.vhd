library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- pragma translate_off
library work;
use work.func_pkg.notx;
-- pragma translate_on

entity sram_2en is 
  generic (
    width_g : positive := 32;
    size_g  : positive := 16
  );
  port (
    clk_i  : in  std_ulogic;
    we_i   : in  std_ulogic_vector(1 downto 0);     
    en_i   : in  std_ulogic;
    addr_i : in  std_ulogic_vector(size_g-1 downto 0);
    di_i   : in  std_ulogic_vector(width_g-1 downto 0);
    do_o   : out std_ulogic_vector(width_g-1 downto 0) := (others=>'0')
  );
end sram_2en;

-- Although this memory is very easy to use in conjunction with Modelsims mem load, it is not
-- supported by many devices (although it comes straight from the library. Many devices give
-- cryptic synthesization errors on this implementation, so it is not the default.
architecture beh of sram_2en is

  type ram_t is array(2**size_g-1 downto 0) of std_ulogic_vector(width_g-1 downto 0);
  type sel_t is array(width_g/32-1 downto 0) of std_ulogic_vector(width_g/2-1 downto 0);

  signal ram : ram_t := (others => (others => '0'));
  signal di  : sel_t := (others => (others => '0'));
  
begin
  process(we_i, di_i, addr_i)
  begin
    if we_i(0) = '1' then
      di(0) <= di_i(width_g/2-1 downto 0);
    else
-- pragma translate_off
      if(notx(addr_i)) then
-- pragma translate_on
        di(0) <= ram(to_integer(unsigned(addr_i)))(width_g/2-1 downto 0);
-- pragma translate_off
      else
        di(0) <= (others=>'X'); -- let undefined state propagate without complaning about
      end if;
-- pragma translate_on
    end if;
    
    if we_i(1) = '1' then
      di(1) <= di_i(width_g-1 downto width_g/2);
    else
-- pragma translate_off
      if(notx(addr_i)) then
-- pragma translate_on
        di(1) <= ram(to_integer(unsigned(addr_i)))(width_g-1 downto width_g/2);
-- pragma translate_off
      else
        di(1) <= (others=>'X'); -- let undefined state propagate without complaning about
      end if;
-- pragma translate_on
    end if;
  end process;

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if en_i = '1' then
        ram(to_integer(unsigned(addr_i))) <= di(1) & di(0);
        do_o                             <= di(1) & di(0);
      end if;
    end if;
  end process;
end beh;

