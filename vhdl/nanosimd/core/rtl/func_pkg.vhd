library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package func_pkg is

  function log2ceil(n : natural) return natural;
  function ispow2(n : natural) return boolean;
  function max(a, b : in integer) return integer;
  function min(a, b : in integer) return integer;
  function count_ones(vec : in std_ulogic_vector) return natural;
  function count_zeros(vec : in std_ulogic_vector) return natural;
  function onehot(vec : in std_ulogic_vector) return std_ulogic_vector;
  function msbset(vec : in std_ulogic_vector) return natural;
  function msbset(vec : in std_ulogic_vector) return std_ulogic_vector;
  function lsbset(vec : in std_ulogic_vector) return natural;
  function lsbset(vec : in std_ulogic_vector) return std_ulogic_vector;

  function v_or(d : std_ulogic_vector) return std_ulogic;
  function v_and(d : std_ulogic_vector) return std_ulogic;
  function is_zero(d : std_ulogic_vector) return std_ulogic;
  function is_not_zero(d : std_ulogic_vector) return std_ulogic;
  function my_conv_integer(a: std_ulogic_vector) return integer;
  function notx(d : std_ulogic_vector) return boolean;
  function compare(a, b : std_ulogic_vector) return std_ulogic;
  function multiply(a, b : std_ulogic_vector) return std_ulogic_vector;
  function sign_extend(value: std_ulogic_vector; fill: std_ulogic; size: positive) return std_ulogic_vector;
  function add(a, b : std_ulogic_vector; ci: std_ulogic) return std_ulogic_vector;
  function increment(a : std_ulogic_vector) return std_ulogic_vector;
  function shift(value : std_ulogic_vector(31 downto 0); shamt: std_ulogic_vector(4 downto 0); s: std_ulogic; t: std_ulogic) return std_ulogic_vector;
  function shift_left(value : std_ulogic_vector(31 downto 0); shamt : std_ulogic_vector(4 downto 0)) return std_ulogic_vector;
  function shift_right(value : std_ulogic_vector(31 downto 0); shamt : std_ulogic_vector(4 downto 0); padding: std_ulogic) return std_ulogic_vector;
  function shift8_left(value : std_ulogic_vector(7 downto 0); shamt : std_ulogic_vector(2 downto 0)) return std_ulogic_vector;
  function shift8_right(value : std_ulogic_vector(7 downto 0); shamt : std_ulogic_vector(2 downto 0); padding: std_ulogic) return std_ulogic_vector;
  function shift16_left(value : std_ulogic_vector(15 downto 0); shamt : std_ulogic_vector(3 downto 0)) return std_ulogic_vector;
  function shift16_right(value : std_ulogic_vector(15 downto 0); shamt : std_ulogic_vector(3 downto 0); padding: std_ulogic) return std_ulogic_vector;

  function signed_mul (a, b : std_ulogic_vector) return std_ulogic_vector;
  function unsigned_mul(a, b : std_ulogic_vector) return std_ulogic_vector;
  function mixed_mul(a, b : std_ulogic_vector; sign : std_ulogic) return std_ulogic_vector;

end package func_pkg;

package body func_pkg is
  ------------------------------------------------------------------------------
  -- log2ceil
  ------------------------------------------------------------------------------
  --function log2ceil (n : natural) return natural is
  --  variable n_bit : unsigned(31 downto 0);
  --begin
  --  if n = 0 then
  --    return 0;
  --  end if;
  --  n_bit := to_unsigned(n-1,32);
  --  for i in 31 downto 0 loop
  --    if n_bit(i) = '1' then
  --      return i+1;
  --    end if;
  --  end loop;  -- i
  --  return 1;
  --end log2ceil;

  function log2ceil(n : natural) return natural is
  begin
     for i in 0 to integer'high loop
        if (2**i >= n) then
           return i;
        end if;
     end loop;
     return 0;
  end log2ceil;

  ------------------------------------------------------------------------------
  -- ispow2
  ------------------------------------------------------------------------------
  function ispow2 (n : natural) return boolean is
  begin
    if(n mod 2 = 0) then
      return true;
    else
      return false;
    end if;
  end function ispow2;

  ------------------------------------------------------------------------------
  -- max
  ------------------------------------------------------------------------------
  function max(a, b : in integer) return integer is
  begin
    if(a > b) then
      return a;
    else
      return b;
    end if;
  end function max;

  ------------------------------------------------------------------------------
  -- min
  ------------------------------------------------------------------------------
  function min(a, b : in integer) return integer is
  begin
    if(a < b) then
      return a;
    else
      return b;
    end if;
  end function min;

  ------------------------------------------------------------------------------
  -- count_ones
  ------------------------------------------------------------------------------
  function count_ones(vec : in std_ulogic_vector) return natural is
    variable cnt : natural;
  begin
    cnt := 0;
    cnt0: for i in vec'range loop
      if(vec(i) = '1') then
        cnt := cnt + 1;
      end if;
    end loop cnt0;
    return cnt;
  end function count_ones;

  ------------------------------------------------------------------------------
  -- count_zeros
  ------------------------------------------------------------------------------
  function count_zeros(vec : in std_ulogic_vector) return natural is
    variable cnt : natural;
  begin
    cnt := 0;
    cnt0: for i in vec'range loop
      if(vec(i) = '0') then
        cnt := cnt + 1;
      end if;
    end loop cnt0;
    return cnt;
  end function count_zeros;

  ------------------------------------------------------------------------------
  -- onehot
  ------------------------------------------------------------------------------
  function onehot(vec : in std_ulogic_vector) return std_ulogic_vector is
    variable vec_out : std_ulogic_vector(vec'length-1 downto 0);
  begin
    vec_out := (others=>'0');
    vec_out(to_integer(unsigned(vec))) := '1';
    return vec_out;   
  end function onehot;

  ------------------------------------------------------------------------------
  -- msbset
  ------------------------------------------------------------------------------
  function msbset(vec : in std_ulogic_vector) return natural is
  begin
    msb0: for i in vec'high downto vec'low loop
      if(vec(i) = '1') then
        return i;
      end if;
     end loop msb0;
    return 0;
  end function msbset;

  ------------------------------------------------------------------------------
  -- msbset
  ------------------------------------------------------------------------------
  function msbset(vec : in std_ulogic_vector) return std_ulogic_vector is
    variable result : std_ulogic_vector(log2ceil(vec'length)-1 downto 0);
  begin
    result := std_ulogic_vector(to_unsigned(msbset(vec), result'length));
    return result;
  end function msbset;

  ------------------------------------------------------------------------------
  -- lsbset
  ------------------------------------------------------------------------------
  function lsbset(vec : in std_ulogic_vector) return natural is
  begin
    lsb0: for i in vec'low to vec'high loop
      if(vec(i) = '1') then
        return i;
      end if;
    end loop lsb0;
    return 0;
  end function lsbset;
  
  ------------------------------------------------------------------------------
  -- lsbset
  ------------------------------------------------------------------------------
  function lsbset(vec : in std_ulogic_vector) return std_ulogic_vector is
    variable result : std_ulogic_vector(log2ceil(vec'length)-1 downto 0);
  begin
    result := std_ulogic_vector(to_unsigned(lsbset(vec), result'length));
    return result;
  end function lsbset;

--------------------------------------------------------------------------------
-- Unary OR reduction
--------------------------------------------------------------------------------
  function v_or(d : std_ulogic_vector) return std_ulogic is
    variable z : std_ulogic;
  begin
    z := '0';
    if(notx(d)) then
      for i in d'range loop
        z := z or d(i);
      end loop;
    end if;
    return z;
  end function v_or;
--------------------------------------------------------------------------------
-- Unary AND reduction
--------------------------------------------------------------------------------
  function v_and(d : std_ulogic_vector) return std_ulogic is
    variable z : std_ulogic;
  begin
    z := '0';
    if(notx(d)) then
      for i in d'range loop
        z := z and d(i);
      end loop;
    end if;
    return z;
  end function v_and;  
--------------------------------------------------------------------------------
-- Check for ones in the vector
--------------------------------------------------------------------------------
  function is_not_zero(d : std_ulogic_vector) return std_ulogic is
    variable z : std_ulogic_vector(d'range);
  begin
    z := (others => '0');
    if(notx(d)) then
      if(d = z) then
        return '0';
      else
        return '1';
      end if;
    else
      return '0';
    end if;
  end function is_not_zero;
--------------------------------------------------------------------------------
-- Check for ones in the vector
--------------------------------------------------------------------------------
  function is_zero(d : std_ulogic_vector) return std_ulogic is
  begin
    return not is_not_zero(d);
  end function is_zero;
--------------------------------------------------------------------------------
-- rewrite conv_integer to avoid modelsim warnings
--------------------------------------------------------------------------------
  function my_conv_integer(a : std_ulogic_vector) return integer is
    variable res : integer range 0 to 2**a'length-1;
  begin
    res := 0;
    if(notx(a)) then
      res := to_integer(unsigned(a));
    end if;
    return res;
  end function my_conv_integer;
--------------------------------------------------------------------------------
--  compare
--------------------------------------------------------------------------------
  function compare(a, b : std_ulogic_vector) return std_ulogic is
    variable z : std_ulogic;
  begin
    if(notx(a & b) and (a = b)) then
      return '1';
    else
      return '0';
    end if;
  end function compare;
--------------------------------------------------------------------------------
-- Unary NOT X test
--------------------------------------------------------------------------------
  function notx(d : std_ulogic_vector) return boolean is
    variable res : boolean;
  begin
    res := true;
-- pragma translate_off
    res := not is_x(d);
-- pragma translate_on
    return (res);
  end function notx;
--------------------------------------------------------------------------------
-- shift
--------------------------------------------------------------------------------
  function shift(value: std_ulogic_vector(31 downto 0); shamt: std_ulogic_vector(4 downto 0); s: std_ulogic; t: std_ulogic) return std_ulogic_vector is
  begin
    if s = '1' then
      -- left arithmetic or logical shift
      return shift_left(value, shamt);
    else
      if(t = '1') then
        -- right arithmetic shift
        return shift_right(value, shamt, value(31));
      else
        -- right logical shift
        return shift_right(value, shamt, '0');
      end if;
    end if;
  end function shift;
--------------------------------------------------------------------------------
-- shift left
--------------------------------------------------------------------------------
  function shift_left(value: std_ulogic_vector(31 downto 0); shamt: std_ulogic_vector(4 downto 0)) return std_ulogic_vector is
    variable result: std_ulogic_vector(31 downto 0);
    variable paddings: std_ulogic_vector(15 downto 0);
  begin
    paddings := (others => '0');
    result := value;
    if(shamt(4) = '1') then result := result(15 downto 0) & paddings(15 downto 0); end if;
    if(shamt(3) = '1') then result := result(23 downto 0) & paddings( 7 downto 0); end if;
    if(shamt(2) = '1') then result := result(27 downto 0) & paddings( 3 downto 0); end if;
    if(shamt(1) = '1') then result := result(29 downto 0) & paddings( 1 downto 0); end if;
    if(shamt(0) = '1') then result := result(30 downto 0) & paddings( 0 );         end if;
    return result;
  end function shift_left;
--------------------------------------------------------------------------------
-- shift left
--------------------------------------------------------------------------------
  function shift16_left(value: std_ulogic_vector(15 downto 0); shamt: std_ulogic_vector(3 downto 0)) return std_ulogic_vector is
    variable result: std_ulogic_vector(15 downto 0);
    variable paddings: std_ulogic_vector(7 downto 0);
  begin
    paddings := (others =>'0');
    result   := value;
    if(shamt(3) = '1') then result := result( 7 downto 0) & paddings( 7 downto 0); end if;
    if(shamt(2) = '1') then result := result(11 downto 0) & paddings( 3 downto 0); end if;
    if(shamt(1) = '1') then result := result(13 downto 0) & paddings( 1 downto 0); end if;
    if(shamt(0) = '1') then result := result(14 downto 0) & paddings( 0 );         end if;
    return result;
  end function shift16_left;
--------------------------------------------------------------------------------
-- shift8 left
--------------------------------------------------------------------------------
  function shift8_left(value: std_ulogic_vector(7 downto 0); shamt: std_ulogic_vector(2 downto 0)) return std_ulogic_vector is
    variable result: std_ulogic_vector(7 downto 0);
    variable paddings: std_ulogic_vector(3 downto 0);
  begin
    paddings := (others=>'0');
    result := value;
    if(shamt(2) = '1') then result := result(3 downto 0) & paddings( 3 downto 0); end if;
    if(shamt(1) = '1') then result := result(5 downto 0) & paddings( 1 downto 0); end if;
    if(shamt(0) = '1') then result := result(6 downto 0) & paddings( 0 );         end if;
    return result;
  end function shift8_left;
--------------------------------------------------------------------------------
-- shift right
--------------------------------------------------------------------------------
  function shift_right(value: std_ulogic_vector(31 downto 0); shamt: std_ulogic_vector(4 downto 0); padding: std_ulogic) return std_ulogic_vector is
    variable result: std_ulogic_vector(31 downto 0);
    variable paddings: std_ulogic_vector(15 downto 0);
  begin
    paddings := (others=>padding);
    result := value;
    if(shamt(4) = '1') then result := paddings(15 downto 0) & result(31 downto 16); end if;
    if(shamt(3) = '1') then result := paddings( 7 downto 0) & result(31 downto  8); end if;
    if(shamt(2) = '1') then result := paddings( 3 downto 0) & result(31 downto  4); end if;
    if(shamt(1) = '1') then result := paddings( 1 downto 0) & result(31 downto  2); end if;
    if(shamt(0) = '1') then result := paddings( 0 )         & result(31 downto  1); end if;
    return result;
  end function shift_right;
--------------------------------------------------------------------------------
-- shift16 right
--------------------------------------------------------------------------------
  function shift16_right(value: std_ulogic_vector(15 downto 0); shamt: std_ulogic_vector(3 downto 0); padding: std_ulogic) return std_ulogic_vector is
    variable result: std_ulogic_vector(15 downto 0);
    variable paddings: std_ulogic_vector(7 downto 0);
  begin
    paddings := (others=>padding);
    result   := value;
    if(shamt(3) = '1') then result := paddings( 7 downto 0) & result(15 downto  8); end if;
    if(shamt(2) = '1') then result := paddings( 3 downto 0) & result(15 downto  4); end if;
    if(shamt(1) = '1') then result := paddings( 1 downto 0) & result(15 downto  2); end if;
    if(shamt(0) = '1') then result := paddings( 0 )         & result(15 downto  1); end if;
    return result;
  end function shift16_right;
--------------------------------------------------------------------------------
-- shift8 right
--------------------------------------------------------------------------------
  function shift8_right(value: std_ulogic_vector(7 downto 0); shamt: std_ulogic_vector(2 downto 0); padding: std_ulogic) return std_ulogic_vector is
    variable result: std_ulogic_vector(7 downto 0);
    variable paddings: std_ulogic_vector(3 downto 0);
  begin
    paddings := (others=>padding);
    result   := value;
    if(shamt(2) = '1') then result := paddings( 3 downto 0) & result(7 downto  4); end if;
    if(shamt(1) = '1') then result := paddings( 1 downto 0) & result(7 downto  2); end if;
    if(shamt(0) = '1') then result := paddings( 0 )         & result(7 downto  1); end if;
    return result;
  end function shift8_right;
--------------------------------------------------------------------------------
-- multiply
--------------------------------------------------------------------------------
  function multiply(a, b: std_ulogic_vector) return std_ulogic_vector is
    variable x: std_ulogic_vector (a'length + b'length - 1 downto 0);
  begin
    x := std_ulogic_vector(signed(a) * signed(b));
    return x(31 downto 0);
  end function multiply;
--------------------------------------------------------------------------------
-- sign extend
--------------------------------------------------------------------------------
  function sign_extend(value: std_ulogic_vector; fill: std_ulogic; size: positive) return std_ulogic_vector is
    variable a: std_ulogic_vector (size - 1 downto 0);
  begin
    a(size - 1 downto value'length) := (others => fill);
    a(value'length - 1 downto 0) := value;
    return a;
  end function sign_extend;
--------------------------------------------------------------------------------
-- add
--------------------------------------------------------------------------------
  function add(a, b : std_ulogic_vector; ci: std_ulogic) return std_ulogic_vector is
    variable x : std_ulogic_vector(a'length downto 0);
  begin
    x := (others => '0');
    if(notx(a & b & ci)) then
      x := std_ulogic_vector(signed(a & '1') + signed(b & ci));
    end if;
    return x(a'length downto 1);
  end function add;
--------------------------------------------------------------------------------
-- increment
--------------------------------------------------------------------------------
  function increment(a : std_ulogic_vector) return std_ulogic_vector is
    variable x : std_ulogic_vector(a'length-1 downto 0);
  begin
    x := (others => '0');
    if(notx(a)) then
      x := std_ulogic_vector(signed(a) + 1);
    end if;
    return x;
  end function increment;
--------------------------------------------------------------------------------
-- signed multiply
--------------------------------------------------------------------------------
  function signed_mul(a, b : std_ulogic_vector) return std_ulogic_vector is
    variable z : std_ulogic_vector(a'length+b'length-1 downto 0);
  begin
-- pragma translate_off
    if notx(a & b) then
-- pragma translate_on
      return(std_ulogic_vector(signed(a) * signed(b)));
-- pragma translate_off
    else
       z := (others =>'X'); return(z);
    end if;
-- pragma translate_on
  end function signed_mul;
--------------------------------------------------------------------------------
-- unsigned multiply
--------------------------------------------------------------------------------
  function unsigned_mul(a, b : std_ulogic_vector) return std_ulogic_vector is
    variable z : std_ulogic_vector(a'length+b'length-1 downto 0);
  begin
-- pragma translate_off
    if notx(a & b) then
-- pragma translate_on
      return(std_ulogic_vector(unsigned(a) * unsigned(b)));
-- pragma translate_off
    else
       z := (others =>'X'); return(z);
    end if;
-- pragma translate_on
  end function unsigned_mul;
--------------------------------------------------------------------------------
-- signed / unsigned multiply
--------------------------------------------------------------------------------
  function mixed_mul(a, b : std_ulogic_vector; sign : std_ulogic) return std_ulogic_vector is
    variable z : std_ulogic_vector(a'length+b'length-1 downto 0);
  begin
-- pragma translate_off
    if notx(a & b) then
-- pragma translate_on
      if sign = '0' then
        return(std_ulogic_vector(unsigned(a) * unsigned(b)));
      else
        return(std_ulogic_vector(signed(a) * signed(b)));
      end if;
-- pragma translate_off
    else
       z := (others =>'X'); return(z);
    end if;
-- pragma translate_on
  end function mixed_mul;
  
end package body func_pkg;

