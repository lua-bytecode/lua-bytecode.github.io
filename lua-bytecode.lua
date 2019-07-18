-- lua-bytecode.lua

-- PUC Lua (5.1, 5.2, 5.3, 5.4.0-alpha) bytecode viewer and converter
-- This module could be run under any Lua 5.1+ having 64-bit "double" floating point Lua numbers

-- Version: 2019-07-18


-- must have "double" Lua numbers
do
   local x = 2^53
   assert(x - (x-1) == 1 and (x+1) - x ~= 1, "Floating point numbers must be 'double'")
end

local function round_float64_to_float32(x)
   if x == 0.0 or x ~= x then
      return x
   end
   local sign = 1.0
   if x < 0.0 then
      sign, x = -sign, -x
   end
   -- float32 limits:
   --    min subnormal positive = 2^-149
   --    max finite positive    = 2^128 - 2^104
   if x > 0.5 * 2^-149 and x < 2^128 then
      local e = 2.0^math.floor(math.log(x)/math.log(2) + 0.5)
      e = (x < e and e * 0.5 or e) * 2^-23
      e = e < 2^-149 and 2^-149 or e
      x = x + 0.5 * e
      local r = x % e
      x = x - (r == 0.0 and x % (e + e) or r)
   end
   if x < 2^-149 then
      return sign * 0.0
   elseif x > 2^128 - 2^104 then
      return sign / 0.0
   else
      return sign * x
   end
end

local serialize_float_value
do

   local function get_shortest_string(str1, str2)
      -- returns the shortest of the two strings (first one on tie)
      return str1 and (str2 and #str2 < #str1 and str2 or str1) or str2
   end

   local function sign_and_three_digits(n, zero)
      -- 42 -> "e+042"
      if n == 0 then
         return zero
      end
      local sign = "+"
      if n < 0 then
         sign, n = "-", -n
      end
      n = tostring(n)
      return "e"..sign..("0"):rep(3 - #n)..n
   end

   local function fraction_to_string(N, D, k, min_degree_float, max_degree_float)
      if (D ~= 1 or k ~= 0) and k >= min_degree_float and k <= max_degree_float and N < 2^20 and D < 2^20 then
         local div, denom = N ~= D and k < 0 and k >= -max_degree_float, ("%.f"):format(D)
         return
            N == 1 and D ~= 1 and k ~= 0 and "2^"..k.." / "..denom
            or (N == D and "" or ("%.f"):format(N)..(D == 1 and "" or "/"..denom)..(k == 0 and "" or " "..(div and "/" or "*").." "))
               ..(k == 0 and "" or "2^"..(div and -k or k))
      end
   end

   function serialize_float_value(the_number, bytes_per_float)
      -- bytes_per_float: 4           = "the_number" is 64-bit float, but it contains a value stored somewhere as 32-bit float
      --                  8 (default) = "the_number" is 64-bit float, and it was loaded from 64-bit float value
      -- The function returns a string (serialized float number) and a predicted_float64 (only when is_float32 = true)
      -- math.type() of loaded string will be "float": the string is either a float literal (having "e" or decimal point) or float Lua expression (such as 3/7 or 2^20)
      if the_number ~= the_number then
         -- nan
         return "0/0"
      end
      local float_number = the_number * 1.0
      assert(float_number == the_number)
      local is_float32 = bytes_per_float == 4
      if is_float32 then
         assert(float_number == round_float64_to_float32(float_number))
      end
      if float_number == 0 then
         -- zero
         return 1/float_number < 0 and "-0.0" or "0.0"
      end
      local sign, positive_float, shortest = "", float_number, "1/0"
      if positive_float < 0 then
         sign, positive_float = "-", -positive_float
      end
      if positive_float ~= 1/0 then
         -- convert the number to string and split the string into mantissa and exponent
         local mant_int, mant_frac, exponent = ("%.17e"):format(positive_float):match"^(%d)%D+(%d+)e([-+]%d+)$"
         local mantissa, trailing_zeroes = (mant_int..mant_frac):match"^([^0].-)(0*)$"
         exponent = assert(tonumber(exponent)) + #trailing_zeroes - #mant_frac
         local test_value = assert(tonumber(mantissa.."e"..exponent))
         assert((is_float32 and round_float64_to_float32(test_value) or test_value) == positive_float)
         -- remove last digits while float value is not affected (example: 0.10000000000000001 -> 0.1)
         repeat
            local truncated_mantissa, incr = mantissa:match"^(.*)(.)$"
            incr = tonumber(incr) > 4
            for _ = 1, 2 do
               local new_exponent, new_mantissa = exponent + 1
               if incr then
                  -- remove the last digit and round up
                  local head, digit, tail = ("0"..truncated_mantissa):match"^(.-)(.)(9*)$"
                  new_mantissa, incr = head:match"^0?(.*)$"..string.char(digit:byte() + 1)
                  new_exponent = new_exponent + #tail
               else
                  -- remove the last digit and round down
                  new_mantissa, incr = truncated_mantissa:match"^(.-)(0*)$"
                  new_exponent = new_exponent + #incr
               end
               test_value = tonumber(new_mantissa.."e"..new_exponent)
               if test_value and (is_float32 and round_float64_to_float32(test_value) or test_value) == positive_float then
                  -- value was preserved after removing the last digit of mantissa
                  mantissa, exponent, truncated_mantissa = new_mantissa, new_exponent
                  break
               end
            end
         until truncated_mantissa
         --local max_digits_before_decimal_point = 9             -- for "good fixed point format"
         --local max_digits_after_decimal_point  = 9             -- for "good fixed point format"
         --local max_digits_in_mantissa = is_float32 and 5 or 9  -- for "good fixed point format"
         --local good_fixed =
         --   #mantissa <= max_digits_in_mantissa
         --   and exponent >= -max_digits_after_decimal_point
         --   and exponent <= max_digits_before_decimal_point - #mantissa
         local good_fixed = exponent >= -9 and #mantissa <= 9 - exponent and #mantissa <= (is_float32 and 5 or 9)
         shortest =
            (good_fixed or exponent < 0 and exponent >= -6 - #mantissa)
            and (
               exponent >= 0 and mantissa..("0"):rep(exponent)..".0"
               or exponent > -#mantissa and mantissa:sub(1, exponent-1).."."..mantissa:sub(exponent)
               or "0."..("0"):rep(-exponent - #mantissa)..mantissa
            )
         if not good_fixed then
            shortest = get_shortest_string(
               -- int53 (int24 for float32)
               positive_float <= (is_float32 and 2^24 or 2^53) and math.floor(positive_float) == positive_float and ("%.f.0"):format(positive_float),
               get_shortest_string(
                  -- fixed point with fractional part
                  shortest,
                  -- scienific notation
                  #mantissa == 1 and mantissa..sign_and_three_digits(exponent + #mantissa - 1, ".0")
                  or mantissa:sub(1, 1).."."..mantissa:sub(2)..sign_and_three_digits(exponent + #mantissa - 1, "")
               )
            )
            -- search for a fraction (example: 0.33333333333333331 -> 1/3)
            local k = math.floor(math.log(positive_float)/math.log(2) + 0.5)
            local e = 2^k
            if positive_float < e then
               k = k - 1
               e = 2^k
            end
            -- e = 2^k;   e <= positive_float < 2*e
            local x, pn, n, pd, d, N, D = positive_float / e - 1.0, 0.0, 1.0, 1.0, 0.0
            local two_wl = is_float32 and 2^9 or 2^20
            repeat
               local Q, q = x + 0.5, x - x % 1.0
               Q = Q - Q % 1.0
               pd, d, D = d, q*d + pd, Q*d + pd
               pn, n, N = n, q*n + pn, Q*n + pn + D
               test_value = N/D * e
               if N >= two_wl then
                  break
               elseif (is_float32 and round_float64_to_float32(test_value) or test_value) == positive_float then
                  -- Fraction found:  N/D * 2^k   where N,D < 2^20    (2^9 for float32)
                  local max_degree_float = is_float32 and 127 or 1023
                  local min_degree_float = is_float32 and -149 or -1074
                  while k > 0 and D % 2.0 == 0 do
                     k, D = k - 1, D / 2.0
                  end
                  while k < 0 and N % 2.0 == 0 do
                     k, N = k + 1, N / 2.0
                  end
                  local frac = fraction_to_string(k > 0 and N * 2^k or N, k < 0 and D * 2^-k or D, 0, 0, 0)
                  if not frac then
                     local dk = k > 0 and 1 or -1
                     local fN = (3 + dk) / 2
                     local fD = 2 / fN
                     repeat
                        frac = fraction_to_string(N, D, k, min_degree_float, max_degree_float) or frac
                        k, N, D = k + dk, N / fN, D / fD
                     until N % 1 + D % 1 ~= 0
                  end
                  shortest = get_shortest_string(shortest, frac)
                  break
               end
               x = 1.0 / (x - q)
            until x >= two_wl or x ~= x
         end
      end
      shortest = sign..shortest
      local float64_value = assert((loadstring or load)("return "..shortest))()
      assert((is_float32 and round_float64_to_float32(float64_value) or float64_value) == float_number)
      return shortest, is_float32 and float64_value
   end

end

local serialize_string_value, is_correct_identifier
do
   local escapings={["\a"]="\\a", ["\b"]="\\b", ["\t"]="\\t", ["\n"]="\\n", ["\v"]="\\v", ["\f"]="\\f", ["\r"]="\\r", ['"']='\\"', ["\\"]="\\\\"}

   function serialize_string_value(str)
      return '"'..str:gsub('[%c"\\]', function(c) return escapings[c] or ("\a%03d"):format(c:byte()) end):gsub("\a(%d%d%d%d)", "\\%1"):gsub("\a0?0?", "\\")..'"'
   end

   local is_keyword = {
      ["and"]=0, ["break"]=0, ["do"]=0, ["else"]=0, ["elseif"]=0, ["end"]=0, ["false"]=0, ["for"]=0, ["function"]=0, ["goto"]=0, ["if"]=0,
      ["in"]=0, ["local"]=0, ["nil"]=0, ["not"]=0, ["or"]=0, ["repeat"]=0, ["return"]=0, ["then"]=0, ["true"]=0, ["until"]=0, ["while"]=0
   }

   function is_correct_identifier(str, Lua_version)
      return
         str:find"^[A-Za-z_][A-Za-z0-9_]*$" and not is_keyword[str]
         or Lua_version == 0x51 and str == "goto"
   end

end


-- instr_modes = Table for disassembling
--    instr_modes[1] = a string (misc info about instruction)
--       1st character (can instruction jump to pc+1 or pc+2)
--          "X" = this instruction can jump to pc+1
--          "V" = this instruction can jump both to pc+1 and pc+2
--          "C" = depends on C: if C>0 then pc+2; if C==0 then pc+1
--          "." = this instruction can jump neither to pc+1 nor to pc+2 (return/tailcall/jmp)
--          "L" = special case for SETLIST
--          (if parameter sBx (in 5.3) or sJ (in 5.4) is present, then instruction can also jump to "pc+1+sBx/sJ" in addition to the options above)
--       2nd character (for determining the pc of initialization of local variable):
--          "-" = registers are not modified by this instruction
--          "A" = only register A is modified
--          "M" = multiple registers might be modified or custom processing is required
--       4th and 5th characters ("B_mode" and "C_mode"):
--          "N" = parameter B/C is not used
--          "U" = parameter B/C is used as unsigned number or somehow else
--          "S" = parameter B/C is used as signed number (s.) = unsigned-127
--          "R" = parameter B/C is a register          (R.)
--          "J" = parameter B/C is a jump offset       (+=sBx) (+=Bx) (+=sJ)
--          "j" = parameter B/C is a jump offset backwards     (-=Bx)
--          "K" = parameter B/C is a register/constant (RK.)     -- take parameter B/C as is
--          "H" = parameter C   is a register/constant (RKC)     -- use k as highest bit of parameter C
--          "k" = parameter B/C is an index of a constant (K.) (KBx)
--       mode "i..k" means k is a parameter in this instruction

-- sC is always integer
-- sB might be either integer or float

local instr_modes = {
   --[[1234]]   MOVE         = {"XA_RNiAB"},                              --@  A B        R(A) := R(B)
   --[[--34]]   BNOT         = {"XA2RNiAB",    "~ "},                     --@  A B        R(A) := ~R(B)
   --[[1234]]   UNM          = {"XA2RNiAB",    "- "},                     --@  A B        R(A) := -R(B)
   --[[1234]]   LEN          = {"XA2RNiAB",    "# "},                     --@  A B        R(A) := length of R(B)
   --[[1234]]   NOT          = {"XA2RNiAB",    "not "},                   --@  A B        R(A) := not R(B)

   --[[1234]]   ADD          = {"XA1KKiABC",   "", " + "},                --@  A B C      R(A) := RK(B) + RK(C)
   --[[---4]]   ADDI         = {"XA7RSiABCk",  " + "},                    --@  A B sC k   R(A) := R(B) + sC              (if k=1 then arguments are swapped)
   --[[---4]]   ADDK         = {"XA5RkiABCk",  " + "},                    --@  A B C k    R(A) := R(B) + K(C)            (if k=1 then arguments are swapped)
   --[[1234]]   MUL          = {"XA1KKiABC",   "", " * "},                --@  A B C      R(A) := RK(B) * RK(C)
   --[[---4]]   MULI         = {"XA7RSiABCk",  " * "},                    --@  A B sC k   R(A) := R(B) * sC              (if k=1 then arguments are swapped)
   --[[---4]]   MULK         = {"XA5RkiABCk",  " * "},                    --@  A B C k    R(A) := R(B) * K(C)            (if k=1 then arguments are swapped)
   --[[1234]]   DIV          = {"XA1KKiABC",   "", " / "},                --@  A B C      R(A) := RK(B) / RK(C)
   --[[---4]]   DIVI         = {"XA7RSiABC",   " / "},                    --@  A B sC     R(A) := R(B) / sC
   --[[---4]]   DIVK         = {"XA5RkiABC",   " / "},                    --@  A B C      R(A) := R(B) / K(C)
   --[[--34]]   IDIV         = {"XA1KKiABC",   "", " // "},               --@  A B C      R(A) := RK(B) // RK(C)
   --[[---4]]   IDIVI        = {"XA7RSiABC",   " // "},                   --@  A B sC     R(A) := R(B) // sC
   --[[---4]]   IDIVK        = {"XA5RkiABC",   " // "},                   --@  A B C      R(A) := R(B) // K(C)
   --[[1234]]   MOD          = {"XA1KKiABC",   "", " % "},                --@  A B C      R(A) := RK(B) % RK(C)
   --[[---4]]   MODI         = {"XA7RSiABC",   " % "},                    --@  A B sC     R(A) := R(B) % sC
   --[[---4]]   MODK         = {"XA5RkiABC",   " % "},                    --@  A B C      R(A) := R(B) % K(C)
   --[[1234]]   SUB          = {"XA1KKiABC",   "", " - "},                --@  A B C      R(A) := RK(B) - RK(C)
   --[[---4]]   SUBI         = {"XA7RSiABC",   " - "},                    --@  A B sC     R(A) := R(B) - sC
   --[[---4]]   SUBK         = {"XA5RkiABC",   " - "},                    --@  A B C      R(A) := R(B) - K(C)
   --[[1234]]   POW          = {"XA1KKiABC",   "", " ^ "},                --@  A B C      R(A) := RK(B) ^ RK(C)
   --[[---4]]   POWI         = {"XA7RSiABC",   " ^ "},                    --@  A B sC     R(A) := R(B) ^ sC
   --[[---4]]   POWK         = {"XA5RkiABC",   " ^ "},                    --@  A B C      R(A) := R(B) ^ K(C)

   --[[--34]]   BAND         = {"XA1KKiABC",   "", " & "},                --@  A B C      R(A) := RK(B) & RK(C)
   --[[---4]]   BANDK        = {"XA6RkiABCk",  " & "},                    --@  A B C k    R(A) := R(B) & K(C):integer    (if k=1 then arguments are swapped)
   --[[--34]]   BOR          = {"XA1KKiABC",   "", " | "},                --@  A B C      R(A) := RK(B) | RK(C)
   --[[---4]]   BORK         = {"XA6RkiABCk",  " | "},                    --@  A B C k    R(A) := R(B) | K(C):integer    (if k=1 then arguments are swapped)
   --[[--34]]   BXOR         = {"XA1KKiABC",   "", " ~ "},                --@  A B C      R(A) := RK(B) ~ RK(C)
   --[[---4]]   BXORK        = {"XA6RkiABCk",  " ~ "},                    --@  A B C k    R(A) := R(B) ~ K(C):integer    (if k=1 then arguments are swapped)

   --[[--34]]   SHL          = {"XA1KKiABC",   "", " << "},               --@  A B C      R(A) := RK(B) << RK(C)
   --[[---4]]   SHLI         = {"XA_RSiABC"},                             --@  A B sC     R(A) := sC << R(B)
   --[[--34]]   SHR          = {"XA1KKiABC",   "", " >> "},               --@  A B C      R(A) := RK(B) >> RK(C)
   --[[---4]]   SHRI         = {"XA_RSiABCk"},                            --@  A B sC k   R(A) := R(B) >> sC             (if k=1 then R(B) << -sC)

   --[[123-]]   CONCAT       = {"XA_RRiABC"},                             --@  A B C      R(A) := R(B).. ... ..R(C)
   --[[---4]]   CONCAT54     = {"XA_UNiAB"},                              --@  A B        R(A) := R(A).. ... ..R(A+B-1)

   --[[123-]]   EQ           = {"V-=KKiABC"},                             --@  A B C      if ((RK(B) == RK(C)) ~= A) then pc++
   --[[123-]]   LE           = {"V-=KKiABC"},                             --@  A B C      if ((RK(B) <= RK(C)) ~= A) then pc++
   --[[123-]]   LT           = {"V-=KKiABC"},                             --@  A B C      if ((RK(B) <  RK(C)) ~= A) then pc++
   --[[---4]]   EQ54         = {"V-<RNiABk"},                             --@  A B k      if ((R(A) == R(B)) ~= k) then pc++
   --[[---4]]   EQI          = {"V-<SUiABCk"},                            --@  A sB C k   if ((R(A) == sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
   --[[---4]]   EQK          = {"V-<kNiABk"},                             --@  A B k      if ((R(A) == K(B)) ~= k) then pc++
   --[[---4]]   LE54         = {"V-<RNiABk"},                             --@  A B k      if ((R(A) <= R(B)) ~= k) then pc++
   --[[---4]]   LEI          = {"V-<SUiABCk"},                            --@  A sB C k   if ((R(A) <= sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
   --[[---4]]   GEI          = {"V-<SUiABCk"},                            --@  A sB C k   if ((R(A) >= sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
   --[[---4]]   LT54         = {"V-<RNiABk"},                             --@  A B k      if ((R(A) <  R(B)) ~= k) then pc++
   --[[---4]]   LTI          = {"V-<SUiABCk"},                            --@  A sB C k   if ((R(A) <  sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
   --[[---4]]   GTI          = {"V-<SUiABCk"},                            --@  A sB C k   if ((R(A) >  sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)

   --[[1234]]   CALL         = {"XM_UUiABC"},                             --@  A B C      R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
   --[[123-]]   VARARG       = {"XM_UNiAB"},                              --@  A B        R(A), R(A+1), ..., R(A+B-2) = vararg
   --[[---4]]   VARARG54     = {"XM_NUiAC"},                              --@  A C        R(A), R(A+1), ..., R(A+C-2) = vararg
   --[[123-]]   TAILCALL     = {".-_UNiAB"},                              --@  A B        return R(A)(R(A+1), ... ,R(A+B-1))
   --[[---4]]   TAILCALL54   = {".-_UUiABCk"},                            --@  A B C k    return R(A)(R(A+1), ... ,R(A+B-1))
   --[[123-]]   RETURN       = {".-_UNiAB"},                              --@  A B        return R(A), ... ,R(A+B-2)
   --[[---4]]   RETURN54     = {".-_UUiABCk"},                            --@  A B C k    return R(A), ... ,R(A+B-2)
   --[[---4]]   RETURN1      = {".-_NNiA"},                               --@  A          return R(A)    -- not a vararg function, no upvalues to close
   --[[---4]]   RETURN0      = {".-_NNi"},                                --@             return         -- not a vararg function, no upvalues to close

   --[[1---]]   LOADNIL51    = {"XM_RNiAB"},                              --@  A B        R(A), R(A+1), ..., R(B) := nil
   --[[-234]]   LOADNIL      = {"XM_UNiAB"},                              --@  A B        R(A), R(A+1), ..., R(A+B) := nil
   --[[1234]]   LOADBOOL     = {"CA_UUiABC"},                             --@  A B C      R(A) := (Bool)B; if (C) pc++
   --[[---4]]   LOADF        = {"XA_UNiAsBx"},                            --@  A sBx      R(A) := (lua_Number)sBx
   --[[---4]]   LOADI        = {"XA_UNiAsBx"},                            --@  A sBx      R(A) := sBx
   --[[1234]]   LOADK        = {"XA2kNiABx"},                             --@  A Bx       R(A) := K(Bx)
   --[[-234]]   LOADKX       = {"XA2NNiA"},                               --@  A          R(A) := K(extra arg)

   --[[1234]]   NEWTABLE     = {"XA_UUiABC"},                             --@  A B C      R(A) := {} (size = B,C)
   --[[1234]]   CLOSURE      = {"XA2UNiABx",   "Function#"},              --@  A Bx       R(A) := closure(KPROTO[Bx])
   --[[-234]]   EXTRAARG     = {"X-3NNiAx",    "EXTRA_ARG "},             --@  Ax         extra (larger) argument for previous opcode
   --[[1234]]   SELF         = {"XM_RHiABCk"},                            --@  A B C k    R(A+1) := R(B); R(A) := R(B)[RK(C):string]
   --[[1234]]   SETLIST      = {"L-_UUiABC"},                             --@  A B C      R(A)[(C-1)*50+i] := R(A+i), 1 <= i <= B   (if C==0 then C=extra arg)
   --[[---4]]   VARARGPREP   = {"X-3NNiA",     "(vararg function has ", " fixed parameters)"}, --@  A          (adjust vararg parameters)
   --[[---4]]   TBC          = {"X-3NNiA",     "mark @R", " 'to-be-closed'"},                  --@  A          mark R(A) "to be closed"
   --[[1--4]]   CLOSE        = {"X-3NNiA",     "CLOSE @R", ",.."},        --@  A          close all upvalues >= R(A)
   --[[-23-]]   JMP          = {".-_JNiAsBx"},                            --@  A sBx      pc+=sBx; if (A) close all upvalues >= R(A - 1)
   --[[1---]]   JMP51        = {".-_JNisBx"},                             --@  sBx        pc+=sBx
   --[[---4]]   JMP54        = {".-_NNisJ"},                              --@  sJ         pc+=sJ

   --[[1234]]   SETUPVAL     = {"X-4UNiAB",    "@Upvalue#", " = @R"},     --@  A B        UpValue[B] := R(A)
   --[[1234]]   GETUPVAL     = {"XA2UNiAB",    "@Upvalue#"},              --@  A B        R(A) := UpValue[B]
   --[[-23-]]   GETTABUP     = {"XA1UKiABC",   "@Upvalue#", "[", "]"},    --@  A B C      R(A) := UpValue[B][RK(C)]
   --[[---4]]   GETTABUP54   = {"XA1UkiABC",   "@Upvalue#", "[", "]"},    --@  A B C      R(A) := UpValue[B][K(C):string]
   --[[-23-]]   SETTABUP     = {"X-3KKiABC",   "@Upvalue#", "[", "] = "}, --@  A B C      UpValue[A][RK(B)] := RK(C)
   --[[---4]]   SETTABUP54   = {"X-3kHiABCk",  "@Upvalue#", "[", "] = "}, --@  A B C k    UpValue[A][K(B):string] := RK(C)
   --[[1---]]   GETGLOBAL    = {"XA2kNiABx",   "GLOBALS[", "]"},          --@  A Bx       R(A) := Gbl[K(Bx)]
   --[[1---]]   SETGLOBAL    = {"X-4kNiABx",   "GLOBALS[", "] = @R"},     --@  A Bx       Gbl[K(Bx)] := R(A)
   --[[1234]]   GETTABLE     = {"XA1RKiABC",   "", "[", "]"},             --@  A B C      R(A) := R(B)[RK(C)]
   --[[---4]]   GETFIELD     = {"XA1RkiABC",   "", "[", "]"},             --@  A B C      R(A) := R(B)[K(C):string]
   --[[---4]]   GETI         = {"XA1RUiABC",   "", "[", "]"},             --@  A B C      R(A) := R(B)[C]
   --[[1234]]   SETTABLE     = {"X-3KHiABCk",  "@R", "[", "] = "},        --@  A B C k    R(A)[RK(B)] := RK(C)
   --[[---4]]   SETFIELD     = {"X-3kHiABCk",  "@R", "[", "] = "},        --@  A B C k    R(A)[K(B):string] := RK(C)
   --[[---4]]   SETI         = {"X-3UHiABCk",  "@R", "[", "] = "},        --@  A B C k    R(A)[B] := RK(C)

   --[[123-]]   TEST         = {"V-_NUiAC"},                              --@  A C        if not (R(A) <=> C) then pc++                      (x<=>y means boolean(x)==boolean(y))
   --[[---4]]   TEST54       = {"V-_NUiAk"},                              --@  A k        if (not R(A) == k) then pc++
   --[[123-]]   TESTSET      = {"VA_RUiABC"},                             --@  A B C      if (R(B) <=> C) then R(A) := R(B) else pc++        (x<=>y means boolean(x)==boolean(y))
   --[[---4]]   TESTSET54    = {"VA_RNiABk"},                             --@  A B k      if (not R(B) == k) then pc++ else R(A) := R(B)

   --[[123-]]   FORPREP      = {".M8JNiAsBx"},                            --@  A sBx      R(A) -= R(A+2); pc+=sBx
   --[[---4]]   FORPREP54    = {"XM8JNiABx"},                             --@  A Bx       if R(A) >? R(A+1) then pc+=Bx+1 else R(A+3) := R(A)
   --[[123-]]   FORLOOP      = {"X-9JNiAsBx"},                            --@  A sBx      R(A) += R(A+2); if R(A) <?= R(A+1) then { R(A+3) := R(A); pc+=sBx }
   --[[---4]]   FORLOOP54    = {"X-9jNiABx"},                             --@  A Bx       R(A) += R(A+2); if R(A) <?= R(A+1) then { R(A+3) := R(A); pc-=Bx }

   --[[1---]]   TFORLOOP51   = {"VM_NUiAC"},                              --@  A C        R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
   --[[---4]]   TFORPREP     = {".-_JNiABx"},                             --@  A Bx       mark R(A+3) "to be closed";  pc+=Bx
   --[[-23-]]   TFORCALL     = {"X-_NUiAC"},                              --@  A C        R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
   --[[---4]]   TFORCALL54   = {"X-_NUiAC"},                              --@  A C        R(A+4), ... ,R(A+3+C) := R(A)(R(A+1), R(A+2))
   --[[-23-]]   TFORLOOP     = {"XM_JNiAsBx"},                            --@  A sBx      if R(A+1) ~= nil then { R(A)=R(A+1); pc+=sBx }
   --[[---4]]   TFORLOOP54   = {"XM_jNiABx"},                             --@  A Bx       if R(A+2) ~= nil then { R(A)=R(A+2); pc-=Bx }
}

local function negative_in_parentheses(n)
   if n < 0 then
      return "("..n..")"
   else
      return tostring(n)
   end
end

local function make_arg(B, B_mode, is_really_an_instruction, k)
   if B_mode == "H" then
      B_mode = "K"
      B = B + (k or 0) * 256
   end
   if B_mode == "K" and B < 256 or B_mode == "R" then
      assert(not is_really_an_instruction or B < 256, "Must be a register")
      return "@R"..B, B
   elseif B_mode == "K" or B_mode == "k" then
      B = B % 256 + 1
      return "@Const#"..B, -B
   elseif B_mode == "S" then
      B = B - 127
      return negative_in_parentheses(B), B
   else
      assert(B_mode == "U")
      return B, B
   end
end

local function regs(from_reg, to_reg, is_LHS)
   -- to_reg = false means "to the top"
   local dogsR = is_LHS and "@@R" or "@R"
   return dogsR..from_reg..(
      from_reg == to_reg and ""
      or from_reg+1 == to_reg and ", "..dogsR..to_reg
      or from_reg+2 == to_reg and ", "..dogsR..(from_reg+1)..", "..dogsR..to_reg
      or to_reg and ",..,"..dogsR..to_reg
      or ",.."
   )
end

local function reserved_size_as_string(B)
   local L = B % 8
   local H = math.floor(B / 8)
   if H == 0 then
      return tostring(L)
   elseif H < 18 then
      return tostring(math.floor((L + 8) * 2^(H-1)))
   else
      return (L == 0 and "" or tostring(1 + L/8).."*").."2^"..(H+2)
   end
end

local function parse_or_convert_bytecode(bytecode_as_string_or_loader, convert_to_enbi)
   -- convert_to_enbi = nil:         parse bytecode and return bytecode object
   --                 = EnBi-string: parse bytecode, convert to this EnBi and return converted bytecode as a string
   local target_bytecode = {}
   local bc_pos = 1
   local block_reader =
      type(bytecode_as_string_or_loader) == "string"
      and
         function(len)
            return bytecode_as_string_or_loader:sub(bc_pos, bc_pos + len - 1)
         end
      or
         bytecode_as_string_or_loader

   local function write_block(str)
      assert(convert_to_enbi)
      table.insert(target_bytecode, str)
   end

   local function read_block(len, mirror_to_target_bytecode)
      local file_offset = bc_pos - 1
      local block = block_reader(len) or ""
      assert(#block == len, "Unexpected end of bytecode")
      bc_pos = bc_pos + math.floor(len)
      if mirror_to_target_bytecode and convert_to_enbi then
         write_block(block)
      end
      return block, file_offset
   end

   local Lua_version, Lua_version_as_string
   local is_LE = false        -- true/false
   local bytes_per_int        -- 4/8
   local bytes_per_pointer    -- 4/8
   local bytes_per_lua_float  -- 4/8
   local bytes_per_lua_int    -- 4/8

   local function write_LE_block(str)
      write_block(convert_to_enbi.is_LE and str or str:reverse())
   end

   local function read_LE_block(len, mirror_to_target_bytecode)
      local s, file_offset = read_block(len)
      local s_LE = is_LE and s or s:reverse()
      if mirror_to_target_bytecode and convert_to_enbi then
         write_LE_block(s_LE)
      end
      return s_LE, file_offset, s
   end

   local function read_byte(mirror_to_target_bytecode)
      return read_block(1, mirror_to_target_bytecode):byte()
   end

   local function write_byte(value)
      write_block(string.char(value))
   end

   local function read_boolean(mirror_to_target_bytecode)
      -- [u8 value]
      local data_in_file, file_offset = read_block(1, mirror_to_target_bytecode)
      local b = data_in_file:byte()
      assert(b < 2, "Wrong boolean value")
      return b > 0, file_offset, data_in_file
   end

   local function write_boolean(value)
      write_block(string.char(value and 1 or 0))
   end

   local function read_unsigned_int_as_double(len)
      local s, result = read_LE_block(len), 0.0
      for pos = len, 1, -1 do
         result = result * 256.0 + s:byte(pos)
      end
      return result
   end

   local function write_double_as_unsigned_int(bytes_in_this_unsigned_int, value)
      assert(bytes_in_this_unsigned_int > 0)
      local s = ""
      for j = 1, bytes_in_this_unsigned_int do
         local low_byte = value % 256.0
         value = (value - low_byte) / 256.0
         s = s..string.char(low_byte)
      end
      write_LE_block(s)
   end

   local function read_int(first_byte)
      -- always mirrors to target bytecode
      if first_byte or Lua_version > 0x53 then
         local value = 0
         first_byte = first_byte or read_byte(true)
         assert(first_byte > 0, "Overlong integer")
         repeat
            assert(value < 2^24, "Integer is beyond int32 range")  -- Lua integer under Fengari is 32-bit
            local b = first_byte or read_byte(true)
            value, first_byte = value * 128 + b
         until b >= 128
         return value - 128
      else
         local value = read_unsigned_int_as_double(bytes_per_int)
         if convert_to_enbi then
            write_double_as_unsigned_int(convert_to_enbi.bytes_per_int, value)
         end
         return math.floor(value)
      end
   end

   local function read_string()
      -- always mirrors to target bytecode
      -- returns "string", file_offset (if string is defined)
      -- returns nil                   (if string is not defined)
      local old_style, len_with_term_zero = Lua_version < 0x53
      if Lua_version <= 0x53 then
         len_with_term_zero = old_style and 0xFF or read_byte(true)
         if len_with_term_zero == 0xFF then
            -- [size_t size_including_term_zero]
            -- string, 00
            len_with_term_zero = read_unsigned_int_as_double(bytes_per_pointer)
            if convert_to_enbi then
               write_double_as_unsigned_int(convert_to_enbi.bytes_per_pointer, len_with_term_zero)
            end
         end
      else
         len_with_term_zero = read_int()
      end
      if len_with_term_zero > 0 then
         local str, file_offset = read_block(len_with_term_zero - 1, true)
         if old_style then
            assert(read_byte(true) == 0, "Wrong string terminator")
         end
         return str, file_offset
      end
   end

   local function double_to_int64_wo_mt(x)
      assert(x % 1.0 == 0.0 and x >= -2^63 and x < 2^63, "Can't convert this float to int64: "..("%.17g"):format(x))
      local low = x % 2^32
      local high = (x - low) / 2^32
      return {high, low}
   end

   local function convert_double_to_lua_int_wo_mt(x)
      -- 32-bit lua integers are stored as array of one float value  {[1]=signed_val32 (-2^31..2^31-1)}
      -- 64-bit lua integers are stored as array of two float values {[1]=signed_high32(-2^31..2^31-1), [2]=unsigned_low32(0..2^32-1)}
      if convert_to_enbi.bytes_per_lua_int == 8 then
         return double_to_int64_wo_mt(x)
      else
         assert(x % 1.0 == 0.0 and x >= -2^31 and x < 2^31, "Can't convert this float to int32: "..("%.17g"):format(x))
         return {x * 1.0}
      end
   end

   local function convert_lua_int_to_double(lua_int_as_array)
      local result = lua_int_as_array[1]
      if lua_int_as_array[2] then
         result = result * 2^32 + lua_int_as_array[2]
      end
      return result
   end

   local function convert_lua_int_to_lua_int(lua_int_as_array)
      local old_size = bytes_per_lua_int
      local new_size = convert_to_enbi.bytes_per_lua_int
      if new_size == old_size then
         return lua_int_as_array
      elseif new_size > old_size then
         return double_to_int64_wo_mt(lua_int_as_array[1])
      else
         local x = convert_lua_int_to_double(lua_int_as_array)
         assert(x >= -2^31 and x < 2^31, "Can't convert this int64 to int32: "..tostring(lua_int_as_array))
         return {x}
      end
   end

   local write_lua_float, write_lua_int, read_lua_float, read_lua_int, read_instruction
   do
      local buffer, bits_in_buffer, str_in_LE_order, str_pos

      local function write_field(field_width, field_value)
         repeat
            local full_byte = field_width + bits_in_buffer >= 8
            local w = full_byte and 8 - bits_in_buffer or field_width
            local two_w = 2^w
            local part = field_value % two_w
            buffer, field_value, field_width = buffer + part * 2^bits_in_buffer, (field_value - part) / two_w, field_width - w
            if full_byte then
               str_in_LE_order, bits_in_buffer, buffer = str_in_LE_order..string.char(buffer), 0, 0.0
            else
               bits_in_buffer = bits_in_buffer + w
            end
         until field_width == 0
      end

      function write_lua_int(value_as_array)
         -- 32-bit lua integers are stored as array of one float value  {[1]=signed_val32 (-2^31..2^31-1)}
         -- 64-bit lua integers are stored as array of two float values {[1]=signed_high32(-2^31..2^31-1), [2]=unsigned_low32(0..2^32-1)}
         buffer, bits_in_buffer, str_in_LE_order = 0.0, 0, ""
         assert(4 * #value_as_array == convert_to_enbi.bytes_per_lua_int)
         for j = #value_as_array, 1, -1 do
            write_field(32, value_as_array[j])
         end
         write_LE_block(str_in_LE_order)
      end

      function write_lua_float(value)
         buffer, bits_in_buffer, str_in_LE_order = 0.0, 0, ""
         local exp_bits, significand_bits = 11, 52
         if convert_to_enbi.bytes_per_lua_float == 4 then
            value, exp_bits, significand_bits = round_float64_to_float32(value), 8, 23
         end
         local max_exp_field = 2^exp_bits - 1.0
         local sign_field, significand, exp_field = 1, 0.0, max_exp_field
         if value ~= value then
            significand = 2^(significand_bits - 1)
         else
            if value == 0.0 and 1.0/value < 0.0 or value < 0.0 then
               value = -value
            else
               sign_field = 0
            end
            if value ~= 1/0 then
               local base_exp, high_bit = (max_exp_field - 1) / 2, 1.0
               local low_exp, exp = 1 - base_exp, math.floor(math.log(value)/math.log(2) + 0.5)
               while value < 2^exp do
                  exp = exp - 1
               end
               assert(value == 0 or exp <= base_exp and exp >= low_exp - significand_bits)
               if exp < low_exp then
                  exp, high_bit = low_exp, 0.0
               end
               exp_field = (exp + base_exp) * high_bit
               assert(exp_field >= 0 and exp_field < max_exp_field)
               value = value / 2^exp - high_bit
               assert(value >= 0.0 and value < 1.0)
               significand = value * 2^significand_bits
               assert(significand % 1.0 == 0.0)
            end
         end
         write_field(significand_bits, significand)
         write_field(exp_bits, exp_field)
         write_field(1, sign_field)
         write_LE_block(str_in_LE_order)
      end

      local function read_field(field_width)
         local result, factor = 0.0, 1.0
         repeat
            if bits_in_buffer == 0 then
               str_pos = str_pos + 1
               buffer = str_in_LE_order:byte(str_pos)
               bits_in_buffer = 8
            end
            local w = field_width < bits_in_buffer and field_width or bits_in_buffer
            local two_w = 2^w
            local part = buffer % two_w
            result, factor, buffer = result + part * factor, factor * two_w, (buffer - part) / two_w
            bits_in_buffer, field_width = bits_in_buffer - w, field_width - w
         until field_width == 0
         return result
      end

      function read_instruction()
         -- always mirrors to target bytecode
         local LE_block, file_offset, data_in_file = read_LE_block(4, true)
         bits_in_buffer, str_in_LE_order, str_pos = 0, LE_block, 0
         local opcode, A, B, C, k
         if Lua_version == 0x54 then
            -- Instruction format:
            --    31..............................0
            --    C(8)..B(8)..k(1)..A(8)..opcode(7)
            opcode = math.floor(read_field(7))
            A = math.floor(read_field(8))
            k = math.floor(read_field(1))
            B = math.floor(read_field(8))
            C = math.floor(read_field(8))
         else
            -- Instruction format:
            --    31........................0
            --    B(9)..C(9)..A(8)..opcode(6)
            opcode = math.floor(read_field(6))
            A = math.floor(read_field(8))
            C = math.floor(read_field(9))
            B = math.floor(read_field(9))
         end
         return opcode, A, B, C, k, file_offset, data_in_file
      end

      function read_lua_float()
         local LE_block, file_offset, data_in_file, result = read_LE_block(bytes_per_lua_float)
         bits_in_buffer, str_in_LE_order, str_pos = 0, LE_block, 0
         local exp_bits = bytes_per_lua_float == 4 and 8 or 11
         local significand_bits = bytes_per_lua_float * 8 - 1 - exp_bits
         local significand = read_field(significand_bits) / 2^significand_bits
         local exp_field = read_field(exp_bits)
         local sign = 1.0 - 2.0 * read_field(1)
         local max_exp_field = 2.0^exp_bits - 1.0
         if exp_field == max_exp_field then  -- inf, nan
            result = (significand == 0.0 and sign or 0.0) / 0.0
         else
            local m = 1.0 % (1.0 + exp_field)
            result = sign * 2.0^(exp_field - m - max_exp_field * 0.5 + 1.5) * (m + significand)
         end
         return result, file_offset, data_in_file
      end

      local int_mt = {
         __tostring =
            function(a)
               local high, low, sign, digits = a[1], a[2], "", ""
               if high < 0.0 then
                  sign = "-"
                  high = -high
                  if low then
                     low = 2^32 - low
                     high = high - 1.0
                  end
               end
               if not low then
                  high, low = 0.0, high
               end
               repeat
                  local r = high % 10.0
                  high = (high - r) / 10.0
                  low = low + r * 2^32
                  r = low % 10.0
                  low = (low - r) / 10.0
                  digits = string.char(r + 48.0)..digits
               until low == 0.0 and high == 0.0
               return sign..digits
            end,
         __index = {
            equals =
               -- usage:
               --    if lua_int:equals(42.0)            then ....
               --    if lua_int:equals(another_lua_int) then ....
               function(self, value)
                  if self[2] then
                     -- lua integers are 64 bit
                     if type(value) == "number" then
                        value = double_to_int64_wo_mt(value)
                     end
                     return self[1] == value[1] and self[2] == value[2]
                  else
                     -- lua integers are 32 bit
                     return self[1] == (type(value) == "number" and value or value[1])
                  end
               end
         }
      }

      local function signed(int32)
         return (int32 + 2^31) % 2^32 - 2^31
      end

      function read_lua_int(LE_block)
         -- 32-bit lua integers are stored as array of one float value  {[1]=signed_val32 (-2^31..2^31-1)}
         -- 64-bit lua integers are stored as array of two float values {[1]=signed_high32(-2^31..2^31-1), [2]=unsigned_low32(0..2^32-1)}
         local file_offset, data_in_file
         if not LE_block then
            LE_block, file_offset, data_in_file = read_LE_block(bytes_per_lua_int)
         end
         bits_in_buffer, str_in_LE_order, str_pos = 0, LE_block, 0
         local low32, result = read_field(32)
         if bytes_per_lua_int == 8 then
            result = {signed(read_field(32)), low32}
         else
            result = {signed(low32)}
         end
         return setmetatable(result, int_mt), file_offset, data_in_file
      end

   end

   local first_3_bytes = read_block(3, true)
   -- Did you know that Lua 5.2+ bytecode file could have optional UTF-8 BOM?  :-)
   if first_3_bytes == '\239\187\191' then
      -- skip UTF-8 BOM
      first_3_bytes = read_block(3, true)
   end
   if first_3_bytes:sub(1, 1) == "#" then
      -- skip comment/shebang
      repeat
         local newline_pos = first_3_bytes:find"\n"
         first_3_bytes = (first_3_bytes..read_block(newline_pos or 3, true)):sub(-3)
      until newline_pos
   end
   assert(first_3_bytes == "\27Lu" and read_block(1, true) == "a", "Wrong bytecode signature.  Only PUC Lua bytecodes are supported.")

   do
      -- [u8 version] Version number (0x52 for Lua 5.2, etc)
      Lua_version = read_byte(true)
      assert(Lua_version ~= 0x54, "Lua versions 5.4.0 work/work2 (published prior to 5.4.0 alpha) are not supported")
      local version_factor = 16
      if not (Lua_version >= 0x10 and Lua_version <= 0x53) then
         Lua_version = read_int(Lua_version)
         assert(Lua_version >= 504 and Lua_version < 2048, "Invalid Lua version specified in bytecode header")  -- first byte should be less than 0x10, hence no collisions up to Lua 20.47
         version_factor = 100
      end
      local major_version = math.floor(Lua_version / version_factor)
      local minor_version = Lua_version % version_factor
      Lua_version = major_version * 16 + minor_version
      Lua_version_as_string = major_version.."."..minor_version
      assert(minor_version < 16 and major_version < 16 and Lua_version >= 0x51 and Lua_version <= 0x54, "Lua version "..Lua_version_as_string.." is not supported")
   end
   -- [u8 impl] Implementation (0 = the bytecode is compatible with the "official" PUC-Rio implementation)
   assert(read_byte(true) == 0, "Bytecode is incompatible with PUC-Rio implementation")

   if convert_to_enbi then
      local version_subset = math.max(Lua_version, 0x52)
      local enbi_pattern = ({
         [0x52] = "^([LB])([48])([48])([048])([048])$",
         [0x53] = "^([LB])([48])([48])([48])([48])$",
         [0x54] = "^([LB])([48])([48])$",
      })[version_subset]
      local e, c_int, c_ptr, lua_int, lua_float = convert_to_enbi:upper():match(enbi_pattern)
      assert(e, "ERROR: Wrong target EnBi string: "..convert_to_enbi.."\nLua "..Lua_version_as_string.." bytecode's EnBi must match the following regex: "..enbi_pattern:gsub("[()]", ""))
      lua_int, lua_float, c_int, c_ptr = tonumber(lua_int), tonumber(lua_float), tonumber(c_int), tonumber(c_ptr)
      if version_subset == 0x52 then
         lua_int   = lua_int   ~= 0 and lua_int   or nil
         lua_float = lua_float ~= 0 and lua_float or nil
         assert(not lua_int ~= not lua_float, "ERROR: Wrong target EnBi string: "..convert_to_enbi.."\nLua "..Lua_version_as_string.." bytecode's EnBi must contain exactly one zero")
      elseif version_subset == 0x54 then
         lua_int, lua_float, c_int, c_ptr = c_int, c_ptr
      end
      convert_to_enbi = {
         is_LE               = e == "L",
         bytes_per_int       = c_int,
         bytes_per_pointer   = c_ptr,
         bytes_per_lua_int   = lua_int,
         bytes_per_lua_float = lua_float
      }
   end

   local instr_names = {}  -- [opcode] = "instruction name"
   do
      local all_instr = "0 MOVE 54 LOADI LOADF 0 LOADK 52+ LOADKX 0 LOADBOOL 51 LOADNIL51 52+ LOADNIL 0 GETUPVAL 54 SETUPVAL 51 GETGLOBAL 52-53 GETTABUP 54 GETTABUP54 0 GETTABLE 54 GETI GETFIELD 51 SETGLOBAL 52-53 SETTABUP 54 SETTABUP54 53- SETUPVAL 0 SETTABLE 54 SETI SETFIELD 0 NEWTABLE SELF 54 ADDI SUBI MULI MODI POWI DIVI IDIVI ADDK SUBK MULK MODK POWK DIVK IDIVK BANDK BORK BXORK SHRI SHLI 0 ADD SUB MUL 52- DIV 0 MOD POW 53+ DIV IDIV BAND BOR BXOR SHL SHR 0 UNM 53+ BNOT 0 NOT LEN 53- CONCAT 54 CONCAT54 CLOSE TBC 51 JMP51 52-53 JMP 54 JMP54 53- EQ LT LE 54 EQ54 LT54 LE54 EQK EQI LTI LEI GTI GEI 53- TEST TESTSET 54 TEST54 TESTSET54 0 CALL 53- TAILCALL 54 TAILCALL54 53- RETURN 54 RETURN54 RETURN0 RETURN1 53- FORLOOP FORPREP 51 TFORLOOP51 52-53 TFORCALL TFORLOOP 54 FORLOOP54 FORPREP54 TFORPREP TFORCALL54 TFORLOOP54 0 SETLIST 51 CLOSE 0 CLOSURE 53- VARARG 54 VARARG54 VARARGPREP 52+ EXTRAARG"
      -- Version mark is applied to all instruction names that follow it until the next version mark is encountered:
      --   "0"        = in all Lua versions
      --   "5x"       = only in 5.x
      --   "5x-5y"    = in all versions from 5.x to 5.y inclusive
      --   "5x+"      = in 5.x and all next versions
      --   "5x-"      = in 5.x and all previous versions
      --   comma means "or", for example: "5x-,5y,5z-5t"
      local version_pattern, idx_dest, is_relevant = "5%d", 0
      version_pattern = version_pattern:gsub("%%", "%%%%")
      for word in all_instr:gmatch"%S+" do
         if word:match"^[%d%p]+$" then
            if word == "0" then
               is_relevant = true
            else
               is_relevant = false
               for range in word:gmatch"[^,]+" do
                  -- "ver-ver"
                  local v1, v2 = range:match((("^(@)%-(@)$"):gsub("@", version_pattern)))
                  -- "ver", "ver+", "ver-"
                  local v3, dir = range:match((("^(@)([-+]?)$"):gsub("@", version_pattern)))
                  assert(v1 or v3, "Invalid version mark "..word)
                  is_relevant = is_relevant
                     or v1 and Lua_version >= tonumber(v1, 16) and Lua_version <= tonumber(v2, 16)
                     or v3 and (
                           dir == "+" and Lua_version >= tonumber(v3, 16)
                        or dir == "-" and Lua_version <= tonumber(v3, 16)
                        or dir == ""  and Lua_version == tonumber(v3, 16)
                     )
               end
            end
         elseif is_relevant then
            instr_names[idx_dest] = word
            idx_dest = idx_dest + 1
         end
      end
   end

   local function read_sizes_of_int_ptr_instr_num()

      if Lua_version <= 0x53 then
         -- [u8 intsize] Size of integers
         bytes_per_int = read_byte()
         assert(bytes_per_int == 4 or bytes_per_int == 8, "Wrong integer size: "..bytes_per_int)
         if convert_to_enbi then
            write_byte(convert_to_enbi.bytes_per_int)
         end

         -- [u8 size_t] Size of pointers
         bytes_per_pointer = read_byte()
         assert(bytes_per_pointer == 4 or bytes_per_pointer == 8, "Wrong pointer size")
         if convert_to_enbi then
            write_byte(convert_to_enbi.bytes_per_pointer)
         end
      end

      -- [u8 instsize] Size of instructions (always 4)
      assert(read_byte(true) == 4, "Wrong instruction size")

      if Lua_version >= 0x53 then
         -- [u8 luaintsize] Size of Lua integers (usually 8)
         bytes_per_lua_int = read_byte()
         assert(bytes_per_lua_int == 4 or bytes_per_lua_int == 8, "Wrong Lua integer size")
         if convert_to_enbi then
            write_byte(convert_to_enbi.bytes_per_lua_int)
         end

         -- [u8 luafloatsize] Size of Lua floats (usually 8)
         bytes_per_lua_float = read_byte()
         assert(bytes_per_lua_float == 4 or bytes_per_lua_float == 8, "Wrong FP number size")
         if convert_to_enbi then
            write_byte(convert_to_enbi.bytes_per_lua_float)
         end
      else
         -- [u8 numsize] Size of Lua numbers (usually 8)
         local bytes_per_lua_number = read_byte()
         assert(bytes_per_lua_number == 4 or bytes_per_lua_number == 8, "Wrong FP number size")
         if convert_to_enbi then
            write_byte(convert_to_enbi.bytes_per_lua_float or convert_to_enbi.bytes_per_lua_int)
         end

         -- [u8 use_int] 0 = numbers are floating-point, 1 = integers instead of floats (for embedded)
         if read_boolean() then
            -- Lua uses integers instead of floats
            bytes_per_lua_int = bytes_per_lua_number
         else
            -- Lua uses floats
            bytes_per_lua_float = bytes_per_lua_number
         end
         if convert_to_enbi then
            write_boolean(convert_to_enbi.bytes_per_lua_int)
         end
      end
   end

   if Lua_version <= 0x52 then
      -- [u8 endian] 0 = Big-endian, 1 = Little-endian
      is_LE = read_boolean()
      if convert_to_enbi then
         write_boolean(convert_to_enbi.is_LE)
      end

      read_sizes_of_int_ptr_instr_num()
   end

   if Lua_version >= 0x52 then
      -- 19 93 0D 0A 1A 0A   Lua magic (used to detect presence of text conversion)
      assert(read_block(6, true) == "\25\147\13\10\26\10", "Wrong file header")
   end

   if Lua_version >= 0x53 then
      read_sizes_of_int_ptr_instr_num()

      -- integer number check value
      local block = read_block(bytes_per_lua_int)
      if not read_lua_int(block:reverse()):equals(0x5678) then
         is_LE = true
         assert(read_lua_int(block):equals(0x5678), "Lua integer check failed.\nPossible reason: Mixed-endian bytecodes are not supported.")
      end
      if convert_to_enbi then
         write_lua_int(convert_double_to_lua_int_wo_mt(0x5678))
      end

      -- float number check value
      assert(read_lua_float() == 370.5, "Lua FP number check failed.\nPossible reasons:\n1. Only IEEE-754 floating point numbers are supported.\n2. Mixed-endian bytecodes are not supported.")
      if convert_to_enbi then
         write_lua_float(370.5)
      end
   end

   local function get_extra_arg(next_instr)
      -- B(9)..C(9)..A(8)..opcode(6)
      -- C(8)..B(8)..k(1)..A(8)..opcode(7)
      local A, B, C, k, opcode, name = next_instr.A, next_instr.B, next_instr.C, next_instr.k, next_instr.opcode, next_instr.name
      local Ax
      if Lua_version == 0x54 then
         Ax = ((C * 256 + B) * 2 + k) * 256 + A
      else
         Ax = (B * 512 + C) * 256 + A
      end
      if Lua_version == 0x51 then
         -- extra arg in Lua 5.1 is used only for SETLIST instruction, and I hope a table size would be < 1e11
         assert(Ax > 7 and Ax < 2^25)  -- hence, we can restrict extra arg value to be a positive int32, because 1e11/50 < 2^31
         Ax = Ax * 64 + opcode
      else
         assert(name == "EXTRAARG")
      end
      return Ax
   end

   local is_global_var_name, next_var_num = {}, 1

   local function generate_next_name()
      local name
      repeat
         name, next_var_num = tostring(next_var_num), next_var_num + 1
         name = "V"..("0"):rep(3 - #name)..name
      until not is_global_var_name[name]
      return name
   end

   local function parse_proto(is_root_proto, parent_upv_qty)
      local max_used_parent_upvalue_index, all_upvalues, upv_qty, source, debug_info = 0, {}  -- debug_info = "stripped"/"included"/nil

      if Lua_version >= 0x53 and is_root_proto then
         upv_qty = read_byte(true)
      end

      if Lua_version ~= 0x52 then
         source = read_string()
         if source then
            debug_info = "included"
         elseif is_root_proto then
            debug_info = "stripped"
         end
      end

      -- [int line_start] debug info -- Line number in source code where chunk starts. 0 for the main chunk.
      local src_line_from = read_int()
      assert(is_root_proto or src_line_from > 0)
      if src_line_from == 0 then  -- for main chunk
         parent_upv_qty = 0
      end

      -- [int line_end] debug info -- Line number in source code where chunk stops. 0 for the main chunk.
      local src_line_to = read_int()
      assert(src_line_to >= src_line_from)

      if Lua_version == 0x51 then
         upv_qty = read_byte(true)
         if src_line_from == 0 then  -- for main chunk
            assert(upv_qty == 0)
         end
         for j = 1, upv_qty do
            all_upvalues[j] = {}
         end
      end

      -- [u8 nparams] -- Number of parameters
      local parameters_qty = read_byte(true)
      if src_line_from == 0 then  -- for main chunk
         assert(parameters_qty == 0)
      end
      -- [u8 varargflags] -- vararg flag
      local is_vararg, has_local_arg, local_arg_contains_table
      if Lua_version == 0x51 then
         -- 5.1 is_vararg flag (bitfield)
         --   1=VARARG_HASARG       -- function has additional local variable "arg" after all other arguments, not counted in "parameters_qty"
         --   2=VARARG_ISVARARG     -- function is vararg (declared as having "..." in its parameter list)
         --   4=VARARG_NEEDSARG     -- local variable "arg" is initialized with an array of arguments with field "n" (this flag is set for every vararg function not using "..." in its body)
         local n = read_byte(true)
         assert(n < 8, "Wrong vararg flags")
         if src_line_from == 0 then  -- for main chunk
            assert(n == 2)
         end
         is_vararg = n % 4 > 1
         has_local_arg = n % 2 > 0
         local_arg_contains_table = n > 3
      else
         -- 5.2+  is_vararg = 0/1
         is_vararg = read_boolean(true)
         if src_line_from == 0 then  -- for main chunk
            assert(is_vararg)
         end
      end

      -- [u8 nregisters] -- number of registers used by this function
      local registers_needed = read_byte(true)

      -- Instructions
      local instr_qty = read_int()
      assert(instr_qty > 0)
      local all_instructions = {}  -- [1..n] = {opcode=, A=, B=, C=, k=, name=, location_in_file="...", src_line_no=, instr_as_text=, instr_as_luac=, is_reachable=}
      -- [int ninstructions]
      for j = 1, instr_qty do
         -- [instsize instruction]
         local opcode, A, B, C, k, file_offset, data_in_file = read_instruction()
         local instr_name = instr_names[opcode]
         local location_in_file = ("+%04X: %02X %02X %02X %02X"):format(file_offset, data_in_file:byte(1, 4))
         all_instructions[j] = {opcode = opcode, A = A, B = B, C = C, k = k, name = instr_name, location_in_file = location_in_file}
      end

      -- Determine unreachable instructions
      do
         local fresh_pc_list = {1}
         all_instructions[1].is_reachable = true
         local jump_dir = {FORPREP54 = 1, FORLOOP54 = -1, TFORPREP = 1, TFORLOOP54 = -1}
         repeat
            local new_fresh_pc_list = {}
            for _, pc in ipairs(fresh_pc_list) do
               local instr = all_instructions[pc]
               local name, deltas = instr.name
               if name then
                  local m = instr_modes[name][1]
                  deltas = {}
                  if Lua_version == 0x54 then
                     local Bx = (instr.C * 256 + instr.B) * 2 + instr.k
                     -- some Lua 5.4 VM instructions can jump +-Bx
                     local dir = jump_dir[name]
                     if dir then
                        table.insert(deltas, dir * Bx)  -- pc+1+-Bx
                     elseif m:match"sJ$" then
                        -- if a Lua 5.4 VM instruction has parameter sJ then it can jump +sJ
                        table.insert(deltas, Bx * 256 + instr.A - 16777215)  -- pc+1+sJ
                     end
                  elseif m:match"sBx$" then
                     -- if a Lua 5.1-5.3 VM instruction has parameter sBx then it can jump +sBx
                     table.insert(deltas, instr.B * 512 + instr.C - 131071)  -- pc+1+sBx
                  end
                  m = m:sub(1, 1)
                  if m == "X" then
                     table.insert(deltas, 0)    -- pc+1
                  elseif m == "V" then
                     table.insert(deltas, 0)    -- pc+1
                     table.insert(deltas, 1)    -- pc+2
                  elseif m == "C" then
                     table.insert(deltas, instr.C > 0 and 1 or 0)
                  elseif m == "L" then
                     -- SETLIST (jump over extraarg)
                     if instr.C == 0 then
                        all_instructions[pc + 1].is_reachable = true -- extraarg is reachable, but it could contain arbitrary opcode on Lua 5.1, so avoid parsing it
                        table.insert(deltas, 1)    -- pc+2
                     else
                        table.insert(deltas, 0)    -- pc+1
                     end
                  end
               else
                  deltas = {0}
               end
               for _, delta in ipairs(deltas) do
                  local reachable_pc = pc + 1 + delta
                  if reachable_pc > 0 and reachable_pc <= instr_qty and not all_instructions[reachable_pc].is_reachable then
                     all_instructions[reachable_pc].is_reachable = true
                     table.insert(new_fresh_pc_list, reachable_pc)
                  end
               end
            end
            fresh_pc_list = new_fresh_pc_list
         until #fresh_pc_list == 0
      end

      -- Determine instructions reading/writing 'top'
      do
         local readers = {CALL = true, TAILCALL = true, TAILCALL54 = true, RETURN = true, RETURN54 = true, SETLIST = true}
         for pc = 1, instr_qty do
            local instr = all_instructions[pc]
            if instr.is_reachable then
               local name, B, C = instr.name, instr.B, instr.C
               if readers[name] and B == 0 then
                  instr.reads_top = true
               end
               if
                  (name == "VARARG54" or name == "CALL") and C == 0
                  or name == "VARARG" and B == 0
               then
                  instr.writes_top = true
               end
            end
         end
      end

      -- [int nconsts]
      local const_qty = read_int()
      local all_consts = {}  -- [1..n] = {type="nil/boolean/string/float/integer", value=, value_as_text=, location_in_file=}
      for j = 1, const_qty do
         -- [u8 type]
         local const_type_id = read_byte(true)
         local base_type_id = const_type_id % 16
         local subtype_id = (const_type_id - base_type_id) / 16
         if base_type_id == 3 or base_type_id == 4 then
            subtype_id = subtype_id - (Lua_version >= 0x54 and 1 or 0)
            assert(subtype_id == 0 or Lua_version >= 0x53 and subtype_id == 1, "Unknown constant type = "..const_type_id)
         end
         local const_type, const_value, const_value_as_text, file_offset, data_in_file
         if base_type_id == 4 then
            -- type 4:  string
            const_type = "string"
            const_value, file_offset = assert(read_string())
            data_in_file = const_value
            assert(const_value, "String is absent")
            const_value_as_text = serialize_string_value(const_value)
         elseif base_type_id == 3 then
            -- type 3:  number
            if subtype_id == 0 and bytes_per_lua_float then
               const_type = "float"
               const_value, file_offset, data_in_file = read_lua_float()
               local predicted_float8
               const_value_as_text, predicted_float8 = serialize_float_value(const_value, bytes_per_lua_float)
               const_value = predicted_float8 or const_value  -- this line replaces float4 with predicted float8
               if convert_to_enbi then
                  if Lua_version <= 0x52 and convert_to_enbi.bytes_per_lua_int then
                     -- convert from Lua 5.1/5.2 with floats to Lua with integers
                     write_lua_int(convert_double_to_lua_int_wo_mt(const_value))
                  else
                     write_lua_float(const_value)
                  end
               end
            else
               const_type = "integer"
               const_value, file_offset, data_in_file = read_lua_int()
               const_value_as_text = tostring(const_value)
               if convert_to_enbi then
                  if Lua_version <= 0x52 and convert_to_enbi.bytes_per_lua_float then
                     -- convert from Lua 5.1/5.2 with integers to Lua with floats
                     write_lua_float(convert_lua_int_to_double(const_value))
                  else
                     write_lua_int(convert_lua_int_to_lua_int(const_value))
                  end
               end
            end
         elseif const_type_id == 1 then
            -- type 1:  boolean
            const_type = "boolean"
            const_value, file_offset, data_in_file = read_boolean(true)
            const_value_as_text = tostring(const_value)
         elseif const_type_id == 0 then
            -- type 0:  nil
            const_type = "nil"
            const_value_as_text = "nil"
            data_in_file, file_offset = read_block(0)
         else
            error("Unknown constant type = "..const_type_id)
         end
         local location_in_file = ("+%04X:Size=%X:"):format(file_offset, #data_in_file)
         for j = 1, math.min(10, #data_in_file) do
            location_in_file = location_in_file..(" %02X"):format(data_in_file:byte(j))
         end
         local target_length = #"+FFFF:Size=A: 01 02 03 04 05 06 07 08 09 0A"
         if #data_in_file > 10 or #location_in_file > target_length then
            location_in_file = location_in_file:sub(1, target_length - 3).."..."
         end
         location_in_file = location_in_file..(" "):rep(target_length - #location_in_file)
         all_consts[j] = {type = const_type, value = const_value, value_as_text = const_value_as_text, location_in_file = location_in_file}
      end

      local function parse_upvalues(qty)
         -- this function is invoked only for 5.2+ bytecodes
         -- [int nupvals]
         upv_qty = read_int()
         assert(upv_qty == (qty or upv_qty))
         for j = 1, upv_qty do
            -- [u8 stack] -- 1 = in registers of enclosing function, 0 = in upvalues of enclosing function
            local in_locals = read_boolean(true)
            -- [u8 register] -- idx (either idx of register or idx of upvalue)
            local index = read_byte(true)
            if not in_locals then
               index = index + 1
               if parent_upv_qty then
                  assert(index <= parent_upv_qty, "Upvalue index is out of range")
               else
                  max_used_parent_upvalue_index = math.max(max_used_parent_upvalue_index, index)
               end
            end
            all_upvalues[j] = {in_locals = in_locals, index = index}    -- [1..n] = {in_locals=true/false, index=, var_name=}
         end
         if src_line_from == 0 then  -- for main chunk
            assert(upv_qty == 1 and all_upvalues[1].in_locals and all_upvalues[1].index == 0)
         end
      end

      if Lua_version >= 0x53 then   -- starting with Lua 5.3, Upvalues and Prototypes were swapped in the bytecode layout
         parse_upvalues(upv_qty)
      end

      -- [int nfunctions]
      local protos_qty = read_int()
      local all_protos = {}
      local max_upvalue_index_used_52 = 0
      for j = 1, protos_qty do
         local proto, debug_info_in_proto, max_upvalue_index_used_52_in_proto = parse_proto(false, upv_qty)
         all_protos[j] = proto
         debug_info = debug_info or debug_info_in_proto
         assert(debug_info == debug_info_in_proto)
         max_upvalue_index_used_52 = math.max(max_upvalue_index_used_52, max_upvalue_index_used_52_in_proto)
      end

      if Lua_version == 0x52 then
         parse_upvalues()
         assert(max_upvalue_index_used_52 <= upv_qty, "Upvalue index is out of range")
         -- [string source] | debug info
         source = read_string()
         local new_debug_info
         if source then
            new_debug_info = "included"
         elseif is_root_proto then
            new_debug_info = "stripped"
         end
         debug_info = debug_info or new_debug_info
         assert(debug_info == (new_debug_info or debug_info))
      end

      -- [int nlines]
      local lines_qty = read_int()
      assert(lines_qty == 0 or lines_qty == instr_qty)
      local new_debug_info = lines_qty > 0 and "included" or "stripped"
      debug_info = debug_info or new_debug_info
      assert(debug_info == new_debug_info)
      if Lua_version <= 0x53 then
         for j = 1, lines_qty do
            -- [int line]
            all_instructions[j].src_line_no = read_int()
         end
      else
         local diff, abs_no = {}, {}
         for j = 1, lines_qty do
            -- [signed byte line_diff]
            local d = read_byte(true)
            diff[j] = d > 127 and d - 256 or d
         end
         -- [int nabslines]
         local abs_lines_qty = read_int()
         assert(
            debug_info == "included" and abs_lines_qty <= lines_qty or
            debug_info == "stripped" and abs_lines_qty == 0
         )
         local prev_pc = 0
         for j = 1, abs_lines_qty do
            -- [int pc]
            local pc = read_int() + 1
            assert(pc > prev_pc and diff[pc] == -128)
            -- [int line]
            local abs_line_no = read_int()
            assert(abs_line_no >= 1)
            abs_no[pc] = abs_line_no
            prev_pc = pc
         end
         local line_no = src_line_from
         for j = 1, lines_qty do
            local d = diff[j]
            line_no = d == -128 and assert(abs_no[j]) or line_no + d
            all_instructions[j].src_line_no = line_no
         end
      end

      -- [int nlocals]
      local locals_qty = read_int()
      assert(
         debug_info == "included" and locals_qty >= parameters_qty + (has_local_arg and 1 or 0) or
         debug_info == "stripped" and locals_qty == 0
      )
      local all_locals = {}
      do
         local regs = {[0] = math.huge}
         for j = 1, locals_qty do
            -- [string name]  debug info
            local local_var_name = assert(read_string())
            -- [int startpc]  It comes into scope at instruction #startpc
            local start_pc = read_int() + 1
            -- [int endpc]    It goes out of scope at instruction #endpc
            local end_pc = read_int()
            -- determine the register where this local lives
            local reg_no = #regs
            while regs[reg_no] < math.min(start_pc, end_pc) do
               reg_no, regs[reg_no] = reg_no - 1
            end
            regs[reg_no + 1] = end_pc
            all_locals[j] = {var_name = local_var_name, start_pc = start_pc, end_pc = end_pc, reg_no = reg_no, def_pc = 0}   -- [1..n] = {start_pc=, end_pc=, var_name=, reg_no=, def_pc=}
         end
      end

      -- [int nupvalnames]
      local upval_names_qty = read_int()
      assert(
         debug_info == "included" and upval_names_qty == upv_qty or
         debug_info == "stripped" and upval_names_qty == 0
      )
      for j = 1, upval_names_qty do
         -- [string name]  debug info
         all_upvalues[j].var_name = assert(read_string())
      end
      if src_line_from == 0 and Lua_version >= 0x52 and not all_upvalues[1].var_name then
         -- if debug info is stripped, set the name of main chunk's upvalue
         all_upvalues[1].var_name = "_ENV"

         local function set_name_for_the_ENV_upvalue(all_protos, upv_index)
            for _, nested_proto in ipairs(all_protos) do
               for j, upv in ipairs(nested_proto.all_upvalues) do
                  if not upv.in_locals and upv.index == upv_index and not upv.var_name then
                     upv.var_name = "_ENV"
                     set_name_for_the_ENV_upvalue(nested_proto.all_protos, j)
                     break
                  end
               end
            end
         end

         -- propagate _ENV upvalue's name recursively down to nested prototypes
         set_name_for_the_ENV_upvalue(all_protos, 1)
      end

      -- Disassembling (preparing fields instr_as_luac and instr_as_text) without resolving upvalue, local, const names
      local data_items_ahead, data_descr = 0
      local last_closure_51_pc, last_closure_51_upvalues, loop_statement
      for pc = 1, instr_qty do
         local instr = all_instructions[pc]
         local A, B, C, k, name, is_reachable = instr.A, instr.B, instr.C, instr.k, instr.name, instr.is_reachable
         local sC, Bx, sBx = C - 127
         if Lua_version <= 0x53 then
            Bx = B * 512 + C
            sBx = Bx - 131071
         else
            Bx = (C * 256 + B) * 2 + k
            sBx = Bx - 65535
         end
         local Ax = Bx * 256 + A
         local sJ = Ax - 16777215
         local instr_as_luac, instr_as_text
         local is_really_an_instruction = data_items_ahead == 0
         if name then
            local instr_info = assert(instr_modes[name])
            local data_1, data_2, data_3, data_4 = instr_info[2] or "", instr_info[3] or "", instr_info[4] or "", instr_info[5] or ""
            local instr_writes_to, group, B_mode, C_mode, mode, k_matters = assert(instr_info[1]:match"^.(.)(.)(.)(.)(i.-)(k?)$")
            k_matters = k and k_matters ~= ""
            --------------------------------------------------------------------------------
            -- Instruction Modes 5.1-5.3:
            --    iABC
            --    iABx    Bx = B..C
            --    iAsBx   signed Bx = unsigned(B..C) - (2^17-1)
            --    iAx     Ax = B..C..A
            --------------------------------------------------------------------------------
            -- Instruction Modes 5.4:
            --            3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
            --            1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
            --    iABCk        C(8)     |      B(8)     |k|     A(8)      |  Opcode(7)  |
            --    iABC         C(8)     |      B(8)     | |     A(8)      |  Opcode(7)  |
            --    iABx               Bx(17)               |     A(8)      |  Opcode(7)  |
            --    iAsBx             sBx (signed)(17)      |     A(8)      |  Opcode(7)  |
            --    iAx                           Ax(25)                    |  Opcode(7)  |
            --    isJ                           sJ(25)                    |  Opcode(7)  |
            --------------------------------------------------------------------------------
            local params, params_luac
            if mode == "iABC" then
               local B_arg, B_arg_luac = make_arg(B, B_mode, is_really_an_instruction)
               local C_arg, C_arg_luac = make_arg(C, C_mode, is_really_an_instruction, k)
               params = {A, B_arg, C_arg}
               params_luac = {A, B_arg_luac, C_arg_luac}
            elseif mode == "iAB" then
               local B_arg, B_arg_luac = make_arg(B, B_mode, is_really_an_instruction)
               params = {A, B_arg}
               params_luac = {A, B_arg_luac}
            elseif mode == "iAC" then
               local C_arg, C_arg_luac = make_arg(C, C_mode, is_really_an_instruction, k)
               params = {A, C_arg}
               params_luac = {A, C_arg_luac}
            elseif mode == "iA" then
               params = {A}
               params_luac = params
            elseif mode == "iABx" then
               if is_really_an_instruction then
                  if B_mode == "J" or B_mode == "j" then
                     local target_pc = pc + 1 + Bx * (B_mode == "J" and 1 or -1)
                     assert(target_pc > 0)
                     params = {A, "<"..target_pc..">"}
                  elseif B_mode == "k" then
                     params = {A, "@Const#"..(Bx + 1)}
                  else
                     assert(B_mode == "U")
                     params = {A, Bx}
                  end
               end
               params_luac = {A, B_mode == "k" and -1-Bx or Bx}
            elseif mode == "iAsBx" then
               if is_really_an_instruction then
                  if B_mode == "J" then
                     local target_pc = pc + 1 + sBx
                     assert(target_pc > 0)
                     params = {A, "<"..target_pc..">"}
                  else
                     assert(B_mode == "U")
                     params = {A, sBx}
                  end
               end
               params_luac = {A, sBx}
            elseif mode == "isBx" then
               if is_really_an_instruction then
                  assert(B_mode == "J")
                  local target_pc = pc + 1 + sBx
                  assert(target_pc > 0)
                  params = {"<"..target_pc..">"}
               end
               params_luac = {sBx}
            elseif mode == "isJ" then
               local target_pc = pc + 1 + sJ
               assert(target_pc > 0)
               params = {"<"..target_pc..">"}
               params_luac = {sJ}
            elseif mode == "iAx" then
               params = {Ax}
               params_luac = params
            elseif mode == "i" then
               params = {}
               params_luac = params
            else
               error"Wrong instruction mode"
            end
            instr_as_luac = name:gsub("5[14]$", "").." "..table.concat(params_luac, " ")..(k_matters and " "..k or "")
            if is_reachable then
               if is_really_an_instruction then
                  if instr.reads_top then
                     assert(all_instructions[pc - 1].writes_top)
                  end
                  if instr.writes_top then
                     assert(all_instructions[pc + 1].reads_top)
                  end
                  local const_with_global_var_name
                  if group == "1" then  -- binary operators
                     assert(mode == "iABC")
                     if name == "GETTABUP" then
                        params[2] = B + 1
                        const_with_global_var_name = C - 255
                     elseif name == "GETTABUP54" then
                        params[2] = B + 1
                        const_with_global_var_name = C + 1
                        local const = all_consts[const_with_global_var_name]
                        assert(const and const.type == "string", "GETTABUP with non-string key")
                     elseif name == "GETFIELD" then
                        local const = all_consts[C + 1]
                        assert(const and const.type == "string", "GETFIELD with non-string key")
                     end
                     instr_as_text = "@@R"..A.." = "..data_1..params[2]..data_2..params[3]..data_3
                  elseif group == "2" then   -- unary operators
                     if name == "LOADKX" then
                        local extra = get_extra_arg(all_instructions[pc + 1])
                        data_items_ahead, data_descr = 1
                        params[2] = "@Const#"..(extra + 1)
                     else
                        assert(mode == "iAB" or mode == "iABx" or mode == "iA")
                        if name == "GETUPVAL" then
                           assert(mode == "iAB")
                           params[2] = B + 1
                        elseif name == "CLOSURE" then
                           assert(mode == "iABx")
                           params[2] = Bx + 1
                           if Lua_version == 0x51 then
                              data_items_ahead, data_descr, last_closure_51_pc, last_closure_51_upvalues = all_protos[Bx + 1].upv_qty, "upvalue_info_51", pc, all_protos[Bx + 1].all_upvalues
                           end
                        elseif name == "GETGLOBAL" then
                           const_with_global_var_name = Bx + 1
                           local const = all_consts[const_with_global_var_name]
                           assert(const and const.type == "string" and is_correct_identifier(const.value, Lua_version), "GETGLOBAL with invalid global variable name")
                        end
                     end
                     instr_as_text = "@@R"..A.." = "..data_1..params[2]..data_2
                  elseif group == "3" or group == "4" then
                     if name == "SETTABUP" then
                        params[1] = A + 1
                        const_with_global_var_name = B - 255
                     elseif name == "SETTABUP54" then
                        params[1] = A + 1
                        const_with_global_var_name = B + 1
                        local const = all_consts[const_with_global_var_name]
                        assert(const and const.type == "string", "SETTABUP with non-string key")
                     elseif name == "SETUPVAL" then
                        params[2] = B + 1
                     elseif name == "SETGLOBAL" then
                        const_with_global_var_name = Bx + 1
                        local const = all_consts[const_with_global_var_name]
                        assert(const and const.type == "string" and is_correct_identifier(const.value, Lua_version), "SETGLOBAL with invalid global variable name")
                     elseif name == "SETFIELD" then
                        local const = all_consts[B + 1]
                        assert(const and const.type == "string", "SETFIELD with non-string key")
                     elseif name == "VARARGPREP" then
                        assert(parameters_qty == A, "VARARGPREP with wrong number of fixed parameters")
                     end
                     -- group == "3" -- the same order of parameters
                     -- group == "4" -- first two parameters swapped
                     local param1, param2 = params[1], params[2]
                     if group == "4" then
                        param1, param2 = param2, param1
                     end
                     instr_as_text = data_1..param1..data_2..(param2 or "")..data_3..(params[3] or "")..data_4
                  elseif group == "5" or group == "6" or group == "7" then
                     -- binary operators with constant(5,6) or immediate integer(7) and sometimes swapped arguments
                     assert(mode == "iABC")
                     if group ~= "7" then
                        local const = assert(all_consts[C + 1])
                        if group == "6" then
                           assert(const.type == "integer", "bitwise operator with non-integer constant")
                        end
                     end
                     local param2, param3 = params[2], params[3]
                     if k_matters and k == 1 then
                        param2, param3 = param3, param2
                     end
                     instr_as_text = "@@R"..A.." = "..param2..data_1..param3
                  elseif name == "SHRI" then
                     -- A B sC k   R(A) := R(B) >> sC                  (if k=1 then R(B) << -sC)
                     if k == 1 then
                        instr_as_text = "@@R"..A.." = @R"..B.." << "..negative_in_parentheses(-sC)
                     else
                        instr_as_text = "@@R"..A.." = @R"..B.." >> "..negative_in_parentheses(sC)
                     end
                  elseif name == "SHLI" then
                     -- A B sC     R(A) := sC << R(B)
                     instr_as_text = "@@R"..A.." = "..negative_in_parentheses(sC).." << @R"..B
                  elseif name == "MOVE" then
                     -- A B     R(A) := R(B)
                     if A == B then
                        instr_as_text = "NOP"
                     else
                        instr_as_text = "@@R"..A.." = @R"..B
                     end
                  elseif name == "LOADF" or name == "LOADI" then
                     -- LOADF     A sBx      R(A) := (lua_Number)sBx
                     -- LOADI     A sBx      R(A) := sBx
                     instr_as_text = "@@R"..A.." = "..sBx..(name == "LOADF" and ".0" or "")
                  elseif name == "TFORPREP" then
                     -- A Bx       mark R(A+3) "to be closed";  pc+=Bx
                     local loop_end_instr = all_instructions[pc + 1 + Bx]
                     assert(loop_end_instr and loop_end_instr.name == "TFORCALL54" and loop_end_instr.A == A, "TFORPREP without TFORCALL")
                     instr_as_text = "mark @R"..(A+3).." 'to-be-closed';  GOTO "..params[2]
                  elseif name == "TFORCALL" or name == "TFORCALL54" then
                     -- TFORCALL       A C        R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
                     -- TFORCALL54     A C        R(A+4), ... ,R(A+3+C) := R(A)(R(A+1), R(A+2))
                     assert(C > 0)
                     local next_instr = all_instructions[pc + 1]
                     assert(next_instr and next_instr.name == "TFORLOOP"..name:sub(9), "TFORCALL without TFORLOOP")
                     local shift = name == "TFORCALL" and 0 or 1
                     local vars = regs(A + 3 + shift, A + 2 + C + shift, true)
                     loop_statement = "for "..vars.." in @R"..A..", @R"..(A+1)..", @R"..(A+2)..(name == "TFORCALL54" and ", @R"..(A+3) or "").." do"
                     instr_as_text = vars.." = @R"..A.."(@R"..(A+1)..", @R"..(A+2)..")"
                  elseif name == "TFORLOOP" or name == "TFORLOOP54" then
                     -- TFORLOOP       A sBx      if R(A+1) ~= nil then { R(A)=R(A+1); pc+=sBx }
                     -- TFORLOOP54     A Bx       if R(A+2) ~= nil then { R(A)=R(A+2); pc-=Bx }
                     do
                        local prev_instr = all_instructions[pc - 1]
                        assert(prev_instr and prev_instr.name == "TFORCALL"..name:sub(9) and prev_instr.A == A-2, "TFORLOOP without TFORCALL")
                     end
                     local jump_offset = name == "TFORLOOP" and sBx or -Bx
                     local loop_start_instr = assert(jump_offset < 0 and all_instructions[pc + jump_offset])
                     do
                        local loop_start_instr_name = loop_start_instr.name
                        local B, C = loop_start_instr.B, loop_start_instr.C
                        assert(
                           (
                              name == "TFORLOOP" and loop_start_instr_name == "JMP" and B * 512 + C - 131071
                              or
                              name == "TFORLOOP54" and loop_start_instr_name == "TFORPREP" and (C * 256 + B) * 2 + loop_start_instr.k
                           ) == -2 - jump_offset, "Wrong TFORLOOP usage"
                        )
                     end
                     loop_start_instr.instr_as_text = loop_start_instr.instr_as_text.." -- "..loop_statement
                     local RA1 = "@R"..(name == "TFORLOOP" and A + 1 or A + 2)
                     instr_as_text = "if "..RA1.." ~= nil then { @@R"..A.." = "..RA1..";  GOTO "..params[2].." } -- end of loop"
                  elseif name == "TFORLOOP51" then
                     -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
                     assert(C > 0 and (all_instructions[pc + 1] or {}).name == "JMP51", "Wrong TFORLOOP usage")
                     local vars = regs(A + 3, A + 2 + C, true)
                     loop_statement = "for "..vars.." in @R"..A..", @R"..(A+1)..", @R"..(A+2).." do"
                     instr_as_text = vars.." = @R"..A.."(@R"..(A+1)..", @R"..(A+2)..");  if @R"..(A+3).." ~= nil then @@R"..(A+2).." = @R"..(A+3).." else GOTO <"..(pc + 2)..">"
                  elseif name == "JMP51" or name == "JMP54" or name == "JMP" then
                     -- JMP    A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)
                     -- JMP51  sBx     pc+=sBx
                     -- JMP54  sJ      pc+=sJ
                     local jump_offset = name == "JMP54" and sJ or sBx
                     local close_needed = name == "JMP" and A > 0
                     local goto_needed = jump_offset ~= 0
                     instr_as_text =
                        (close_needed or goto_needed)
                        and
                           (close_needed and "CLOSE @R"..(A-1)..",.." or "")
                           ..(close_needed and goto_needed and ";  " or "")
                           ..(goto_needed and "GOTO "..params[#params] or "")
                        or
                           "NOP"
                     if (all_instructions[pc - 1] or {}).name == "TFORLOOP51" then
                        local loop_start_instr = all_instructions[pc + sBx]
                        assert(sBx < 0 and loop_start_instr and loop_start_instr.name == "JMP51"
                           and loop_start_instr.B * 512 + loop_start_instr.C - 131071 == -2 - sBx, "Wrong TFORLOOP usage")
                        loop_start_instr.instr_as_text = loop_start_instr.instr_as_text.." -- "..loop_statement
                        instr_as_text = instr_as_text.." -- end of loop"
                     end
                  elseif name == "LOADNIL" or name == "LOADNIL51" then
                     -- LOADNIL    A B     R(A), R(A+1), ..., R(A+B) := nil
                     -- LOADNIL51  A B     R(A) := ... := R(B) := nil
                     instr_as_text = regs(A, name == "LOADNIL" and A + B or B, true).." = nil"
                  elseif name == "SELF" then
                     -- A B C   R(A+1) := R(B); R(A) := R(B)[RK(C):identifier]
                     assert(C > 255 or k == 1, "SELF with not-a-constant as method name")
                     local const = all_consts[C + (k or 0) * 256 - 255]
                     assert(const and const.type == "string" and is_correct_identifier(const.value, Lua_version), "SELF with invalid method name")
                     instr_as_text = "@@R"..(A+1).." = @R"..B..";  @@R"..A.." = @R"..B.."["..params[3].."]   -- prepare for @R"..B..":"..const.value.."()"
                  elseif group == "8" then
                     -- FORPREP      A sBx      R(A)-=R(A+2); pc+=sBx
                     -- FORPREP54    A Bx       if R(A) >? R(A+1) then pc+=Bx+1 else R(A+3) := R(A)
                     local jump_offset = name == "FORPREP" and sBx or Bx
                     assert(jump_offset >= 0 and (all_instructions[pc + 1 + jump_offset] or {}).name == "FORLOOP"..name:sub(8), "FORPREP without FORLOOP")
                     local param_2 = params[2]
                     if name == "FORPREP" then
                        instr_as_text = "@@R"..A.." -= @R"..(A+2)..";  GOTO "..param_2
                     else
                        param_2 = param_2:gsub("%d+", function(n) return tostring(1 + tonumber(n)) end)
                        instr_as_text = "if @R"..A.." >? @R"..(A+1).." then GOTO "..param_2.." else @@R"..(A+3).." = @R"..A
                     end
                     instr_as_text = instr_as_text.." -- for @@R"..(A+3).." = @R"..A..", @R"..(A+1)..", @R"..(A+2).." do"
                  elseif group == "9" then
                     -- FORLOOP      A sBx      R(A)+=R(A+2); if R(A) <?= R(A+1) then { R(A+3)=R(A); pc+=sBx }
                     -- FORLOOP54    A Bx       R(A)+=R(A+2); if R(A) <?= R(A+1) then { R(A+3)=R(A); pc-=Bx  }
                     local jump_offset = name == "FORLOOP" and sBx or -Bx
                     local instr = all_instructions[pc + jump_offset]
                     assert(jump_offset < 0 and instr and instr.name == "FORPREP"..name:sub(8)
                        and (name == "FORLOOP" and instr.B * 512 + instr.C - 131071 or (instr.C * 256 + instr.B) * 2 + instr.k) == -1 - jump_offset,
                        "FORLOOP without FORPREP")
                     instr_as_text = "@@R"..A.." += @R"..(A+2)..";  if @R"..A.." <?= @R"..(A+1).." then { @@R"..(A+3).." = @R"..A..";  GOTO "..params[2].." } -- end of loop"
                  elseif name == "TESTSET" or name == "TESTSET54" then
                     -- TESTSET    A B C      if (R(B) <=> C) then R(A) := R(B) else pc++
                     -- TESTSET54  A B k      if (not R(B) == k) then pc++ else R(A) := R(B)
                     local cond = name == "TESTSET" and C or k
                     assert(cond < 2 and all_instructions[pc + 1].name:sub(1, 3) == "JMP")
                     instr_as_text = "if "..(cond > 0 and "" or "not ").."@R"..B.." then @@R"..A.." = @R"..B.." else GOTO <"..(pc + 2)..">"
                  elseif name == "TEST" or name == "TEST54" then
                     -- TEST    A C        if not (R(A) <=> C) then pc++
                     -- TEST54  A k        if (not R(A) == k) then pc++
                     local cond = name == "TEST" and C or k
                     assert(cond < 2 and all_instructions[pc + 1].name:sub(1, 3) == "JMP")
                     instr_as_text = "if "..(cond > 0 and "not " or "").."@R"..A.." then GOTO <"..(pc + 2)..">"
                  elseif group == "=" then
                     -- EQ   A B C   if ((RK(B) == RK(C)) ~= A) then pc++
                     -- LE   A B C   if ((RK(B) <= RK(C)) ~= A) then pc++
                     -- LT   A B C   if ((RK(B) <  RK(C)) ~= A) then pc++
                     assert(A < 2 and all_instructions[pc + 1].name:sub(1, 3) == "JMP")
                     local operation = assert(({LE = " <= ", LT = " < ", EQ = " == "})[name])
                     local left, right = params[2], params[3]
                     if B > 255 and C <= 255 then
                        left, right = right, left
                        operation = operation:gsub(".", {[">"] = "<", ["<"] = ">"})
                     end
                     local opn, cls = "", ""
                     if A > 0 then
                        if name == "EQ" then
                           operation = " ~= "
                        else
                           opn, cls = "not (", ")"
                     end
                     end
                     instr_as_text = "if "..opn..left..operation..right..cls.." then GOTO <"..(pc + 2)..">"
                  elseif group == "<" then
                     -- EQ54     A B k      if ((R(A) == R(B)) ~= k) then pc++
                     -- EQI      A sB C k   if ((R(A) == sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
                     -- EQK      A B k      if ((R(A) == K(B)) ~= k) then pc++
                     -- LE54     A B k      if ((R(A) <= R(B)) ~= k) then pc++
                     -- LEI      A sB C k   if ((R(A) <= sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
                     -- GEI      A sB C k   if ((R(A) >= sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
                     -- LT54     A B k      if ((R(A) <  R(B)) ~= k) then pc++
                     -- LTI      A sB C k   if ((R(A) <  sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
                     -- GTI      A sB C k   if ((R(A) >  sB  ) ~= k) then pc++   (C==0: sB is integer, C==1: sB is float)
                     assert(all_instructions[pc + 1].name == "JMP54")
                     local name2 = name:sub(1, 2)
                     local operation = assert(({LE = " <= ", GE = " >= ", LT = " < ", GT = " > ", EQ = " == "})[name2])
                     local opn, cls = "", ""
                     if k > 0 then
                        if name2 == "EQ" then
                           operation = " ~= "
                        else
                           opn, cls = "not (", ")"
                        end
                     end
                     local param_2 = params[2]
                     if params[3] == 1 then   -- C==1 means sB is float
                        param_2 = param_2:gsub("%d+", "%0.0")
                     end
                     instr_as_text = "if "..opn.."@R"..A..operation..param_2..cls.." then GOTO <"..(pc + 2)..">"
                  elseif name == "NEWTABLE" then
                     instr_as_text = "@@R"..A.." = {}  arr:"..reserved_size_as_string(B)..", hash:"..reserved_size_as_string(C)
                  elseif name == "SETLIST" then
                     -- A B C   R(A)[(C-1)*50+i] := R(A+i), 1 <= i <= B
                     --  (*)   If (C == 0) then next 'instruction' is:  (for 5.1) real C,  (for 5.2+) EXTRAARG(real C).
                     if C == 0 then
                        C = get_extra_arg(all_instructions[pc + 1])
                        data_items_ahead, data_descr = 1, Lua_version == 0x51 and "extra_arg_51"
                     end
                     --  (*)   If (B == 0) then B = 'top'
                     -- B = 0    R(A)[((C-1)*50+1)..] = R((A+1)..)
                     -- B > 0    R(A)[((C-1)*50+1)..((C-1)*50+B)] = R((A+1)..(A+B))
                     local from_idx = (C-1)*50+1
                     instr_as_text = "@@R"..A.."["..from_idx.."]"..(
                        B == 0 and ",.."
                        or B == 1 and ""
                        or B == 2 and ", @@R"..A.."["..(from_idx + 1).."]"
                        or B == 3 and ", @@R"..A.."["..(from_idx + 1).."]"..", @@R"..A.."["..(from_idx + 2).."]"
                        or ",..,@@R"..A.."["..(from_idx + B - 1).."]"
                     ).." = "..regs(A+1, B>0 and A+B)
                  elseif name == "CONCAT" or name == "CONCAT54" then
                     -- CONCAT    A B C   R(A) := R(B).. ... ..R(C)
                     -- CONCAT54  A B     R(A) := R(A).. ... ..R(A + B - 1)
                     local from, to = B, C
                     if name == "CONCAT54" then
                        from, to = A, A + B - 1
                     end
                     assert(to > from)
                     local t = {}
                     for j = from, to do
                        table.insert(t, "@R"..j)
                     end
                     instr_as_text = "@@R"..A.." = "..table.concat(t, "..")
                  elseif name == "CALL" then
                     --  A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
                     --     (*)  If (B == 0) then B = top.
                     --          If (C == 0) then 'top' is set to last_result+1, so next open instruction (CALL, RETURN, SETLIST) may use 'top'.
                     instr_as_text =
                        (C == 1 and "" or regs(A, C > 0 and A + C - 2, true).." = ")
                        .."@R"..A.."("..(B == 1 and "" or regs(A + 1, B > 0 and A + B - 1))..")"
                  elseif name == "TAILCALL" or name == "TAILCALL54" then
                     -- TAILCALL      A B        return R(A)(R(A+1), ... ,R(A+B-1))
                     -- TAILCALL54    A B C k    return R(A)(R(A+1), ... ,R(A+B-1))
                     if name == "TAILCALL54" then
                        -- (*) In instructions OP_RETURN/OP_TAILCALL, 'k' specifies that the
                        -- function either builds upvalues, which may need to be closed, or is
                        -- vararg, which must be corrected before returning. When 'k' is true,
                        -- C > 0 means the function is vararg and (C - 1) is its number of
                        -- fixed parameters.
                        if k > 0 then
                           assert(C == (is_vararg and parameters_qty + 1 or 0), "Wrong parameter C in TAILCALL")
                        else
                           assert(not is_vararg, "TAILCALL with k=0 inside vararg function")
                        end
                     end
                     instr_as_text = "RETURN @R"..A.."("..(B == 1 and "" or regs(A+1, B>0 and A+B-1))..")"
                  elseif name == "RETURN" or name == "RETURN54" then
                     -- RETURN      A B        return R(A), ... ,R(A+B-2)
                     -- RETURN54    A B C k    return R(A), ... ,R(A+B-2)    k и C используются только для проверок
                     --    (*) If (B == 0) then return up to 'top'.
                     if name == "RETURN54" then
                        if k > 0 then
                           assert(C == (is_vararg and parameters_qty + 1 or 0), "Wrong parameter C in RETURN")
                        else
                           assert(not is_vararg, "RETURN with k=0 inside vararg function")
                        end
                     end
                     instr_as_text = "RETURN"..(B == 1 and "" or " "..regs(A, B>0 and A+B-2))
                  elseif name == "RETURN0" or name == "RETURN1" then
                     -- RETURN0                return
                     -- RETURN1     A          return R(A)
                     assert(not is_vararg, name.." inside vararg function")
                     instr_as_text = "RETURN"..(name == "RETURN1" and " @R"..A or "")
                  elseif name == "VARARG" or name == "VARARG54" then
                     -- VARARG    A B        R(A), R(A+1), ..., R(A+B-2) = vararg
                     -- VARARG54  A C        R(A), R(A+1), ..., R(A+C-2) = vararg
                     --   (*) If (B/C == 0) then use actual number of varargs and set top (like in CALL with C == 0).
                     local n = (name == "VARARG54" and C or B) - 1
                     instr_as_text = n == 0 and "NOP" or (regs(A, n > 0 and A + (n-1), true).." = ...")
                  elseif name == "LOADBOOL" then
                     --@  A B C   R(A) := (Bool)B; if (C) pc++
                     assert(B < 2 and C < 2)
                     if C > 0 then
                        local next_instr = all_instructions[pc + 1]
                        assert(next_instr.name == name and next_instr.A == A and next_instr.B + B == 1 and next_instr.C == 0)
                     end
                     instr_as_text = "@@R"..A.." = "..tostring(B > 0)..(C > 0 and ";  GOTO <"..(pc + 2)..">" or "")
                  else
                     error("There is no disassembly code for instruction "..name)
                  end
                  -- process global var names
                  if (const_with_global_var_name or 0) > 0 then
                     local const = all_consts[const_with_global_var_name]
                     if const.type == "string" then
                        local name = const.value   -- this is either name of some global var or string key of some upvalue table
                        if name:match"^V%d%d%d+$" then
                           is_global_var_name[name] = true  -- don't use this name as local variable name or upvalue name
                        end
                     end
                  end
                  if locals_qty > 0 and instr_writes_to ~= "-" then
                     -- searching for pc of initialization for every local variable
                     local assignment_pc, regs_from, regs_to = pc, A, A
                     if instr_writes_to == "M" then
                        if name == "SELF" then
                           -- SELF       -- A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]
                           regs_to = A + 1
                        elseif name == "LOADNIL" then
                           -- LOADNIL    -- A B     R(A), R(A+1), ..., R(A+B) := nil
                           regs_to = A + B
                        elseif name == "LOADNIL51" then
                           -- LOADNIL51  -- A B     R(A) := ... := R(B) := nil
                           regs_to = B
                        elseif name == "VARARG" then
                           -- VARARG     -- A B     R(A), R(A+1), ..., R(A+B-2) = vararg
                           regs_to = A + B - 2
                        elseif name == "VARARG54" or name == "CALL" then
                           -- VARARG     -- A C     R(A), R(A+1), ..., R(A+C-2) = vararg
                           -- CALL       -- A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
                           regs_to = A + C - 2
                        elseif name == "FORPREP" or name == "FORPREP54" then
                           -- FORPREP      A sBx      R(A)-=R(A+2); pc+=sBx
                           -- FORPREP54    A Bx       if R(A) >? R(A+1) then pc+=Bx+1 else R(A+3) := R(A)
                           -- (loop body)
                           -- FORLOOP      A sBx      R(A)+=R(A+2); if R(A) <?= R(A+1) then { R(A+3)=R(A); pc+=sBx }
                           -- FORLOOP54    A Bx       R(A)+=R(A+2); if R(A) <?= R(A+1) then { R(A+3)=R(A); pc-=Bx  }
                           regs_from = A + 3
                           regs_to = regs_from
                        elseif name == "TFORLOOP" then
                           -- JMP
                           -- (loop body)
                           -- TFORCALL     A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
                           -- TFORLOOP     A sBx   if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }
                           assignment_pc, regs_from, regs_to = pc + sBx, A + 1, A + all_instructions[pc - 1].C
                        elseif name == "TFORLOOP54" then
                           -- TFORPREP     A Bx       mark R(A+3) "to be closed";  pc+=Bx
                           -- (loop body)
                           -- TFORCALL54   A C        R(A+4), ... ,R(A+3+C) := R(A)(R(A+1), R(A+2))
                           -- TFORLOOP54   A Bx       if R(A+2) ~= nil then { R(A)=R(A+2); pc-=Bx }
                           assignment_pc, regs_from, regs_to = pc - Bx, A + 2, A + 1 + all_instructions[pc - 1].C
                        elseif name == "TFORLOOP51" then
                           -- JMP
                           -- (loop body)
                           -- TFORLOOP51 -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
                           -- JMP
                           local next_instr = all_instructions[pc + 1]
                           assignment_pc, regs_from, regs_to = pc + next_instr.B * 512 + next_instr.C - 131070, A + 3, A + 2 + C
                        else
                           error(name)
                        end
                     end
                     for j = 1, locals_qty do
                        local loc = all_locals[j]
                        local loc_reg = loc.reg_no
                        if
                           loc_reg >= regs_from and loc_reg <= regs_to and assignment_pc < loc.start_pc and assignment_pc > loc.def_pc
                           and not (name == "LOADBOOL" and loc.def_pc == pc - 1 and pc > 1 and all_instructions[pc - 1].name == name)
                        then
                           loc.def_pc = assignment_pc
                        end
                     end
                  end
               elseif data_descr == "upvalue_info_51" then
                  assert(name == "GETUPVAL" or name == "MOVE")
                  local upv_info = last_closure_51_upvalues[pc - last_closure_51_pc]
                  if name == "MOVE" then
                     upv_info.in_locals = true
                     upv_info.index = B
                  else
                     assert(B < upv_qty, "Upvalue index is out of range")
                     upv_info.index = B + 1
                  end
               end
            end
         else
            instr_as_luac = instr.opcode.." "..A.." "..B.." "..C..(k and " "..k or "")
            if is_really_an_instruction then
               instr_as_text = "UNRECOGNIZED opcode="..instr.opcode..", A="..A..", B="..B..", C="..C..(k and ", k="..k or "")
            end
         end
         if not is_reachable then
            instr_as_text = "(unreachable instruction)"
         elseif not is_really_an_instruction then
            data_items_ahead = data_items_ahead - 1
            instr_as_text = "(data for previous instruction)"
            if data_descr == "extra_arg_51" then
               instr_as_luac = ("%.f"):format(Ax * 64.0 + instr.opcode)
            end
         end
         instr.instr_as_luac = instr_as_luac
         instr.instr_as_text = instr_as_text
      end

      return
         {   -- proto object
            source = source,
            src_line_from = src_line_from,
            src_line_to = src_line_to,
            parameters_qty = parameters_qty,            -- The local variable "arg" (exists when VARARG_NEEDSARG=1) is not counted in parameters_qty
            is_vararg = is_vararg,
            has_local_arg = has_local_arg,
            local_arg_contains_table = local_arg_contains_table,
            registers_needed = registers_needed,
            const_qty = const_qty,
            all_consts = all_consts,                  -- [1..n] = {type="nil/boolean/string/float/integer", value=, value_as_text=, location_in_file=}
            upv_qty = upv_qty,
            all_upvalues = all_upvalues,              -- [1..n] = {in_locals=true/false, index=, var_name=}
            locals_qty = locals_qty,
            all_locals = all_locals,                  -- [1..n] = {var_name=, start_pc=, end_pc=, reg_no=, def_pc=}
            instr_qty = instr_qty,
            all_instructions = all_instructions,      -- [1..n] = {opcode=, A=, B=, C=, k=, name=, location_in_file="...", src_line_no=, instr_as_text=, instr_as_luac=, is_reachable=}
            protos_qty = protos_qty,
            all_protos = all_protos,                  -- [1..n] = Proto object
         },
         debug_info,
         max_used_parent_upvalue_index
   end

   local root_proto, debug_info = parse_proto(true)

   return convert_to_enbi and table.concat(target_bytecode) or {
      -- bytecode object
      Lua_version = Lua_version,
      Lua_version_as_string = Lua_version_as_string,
      enbi = {
         is_LE = is_LE,
         bytes_per_int = bytes_per_int,
         bytes_per_pointer = bytes_per_pointer,
         bytes_per_lua_int = bytes_per_lua_int,
         bytes_per_lua_float = bytes_per_lua_float,
      },
      root_proto = root_proto,
      debug_info_is_stripped = debug_info == "stripped"
   }
end

------------------------------ DISPLAY ------------------------------

local function rpad(s, len, filler)
   filler = filler or " "
   s = tostring(s)
   while #s < len do
      s = s..filler
   end
   return s
end

local function lpad(s, len, filler)
   filler = filler or " "
   s = tostring(s)
   while #s < len do
      s = filler..s
   end
   return s
end


local function print_proto_object(Print, proto_object, Lua_version, depth, debug_info_is_stripped, subfunc_idx, path)
   -- Fields of Proto object:
   --    source
   --    src_line_from
   --    src_line_to
   --    parameters_qty             -- The local variable "arg" (exists when VARARG_NEEDSARG=1) is not counted in parameters_qty
   --    is_vararg
   --    has_local_arg
   --    local_arg_contains_table
   --    registers_needed
   --    const_qty
   --    all_consts                -- [1..n] = {type="nil/boolean/string/float/integer", value=, value_as_text=, location_in_file=}
   --    upv_qty
   --    all_upvalues              -- [1..n] = {in_locals=true/false, index=, var_name=}
   --    locals_qty
   --    all_locals                -- [1..n] = {start_pc=, end_pc=, var_name=, reg_no=, def_pc=}
   --    instr_qty
   --    all_instructions          -- [1..n] = {opcode=, A=, B=, C=, k=, name=, location_in_file="...", src_line_no=, instr_as_text=, instr_as_luac=, is_reachable=}
   --    protos_qty
   --    all_protos                -- [1..n] = Proto object
   local indent = ("\t"):rep(depth)
   Print()
   if depth > 0 then
      path = path.."."..subfunc_idx
      Print(indent.."************ Function#"..subfunc_idx.."   (full path: "..path..")")
   else
      path = "main"
      Print(indent.."************ Main Function")
   end
   if proto_object.source then
      Print(indent.."Source: "..proto_object.source:gsub("%s+", " "))
   end
   if proto_object.src_line_from ~= 0 then
      Print(indent.."Source line#: "..proto_object.src_line_from
         ..(proto_object.src_line_from == proto_object.src_line_to and "" or ".."..proto_object.src_line_to)
      )
   end
   Print(indent.."Parameters: "..proto_object.parameters_qty)
   Print(indent.."Is vararg: "..(proto_object.is_vararg and "yes" or "no"))
   if proto_object.is_vararg and Lua_version == 0x51 then
      Print(indent.."Local variable 'arg': "..(proto_object.has_local_arg
         and (proto_object.local_arg_contains_table and "contains array of arguments" or "contains nil") or "absent")
      )
   end
   Print(indent.."VM registers needed: "..proto_object.registers_needed)
   Print(indent.."Constants: "..proto_object.const_qty)
   for j = 1, proto_object.const_qty do
      local const = proto_object.all_consts[j]
      Print(indent.."  "..const.location_in_file..rpad("   Const#"..j, 12).." "..rpad(const.type, 9).." "..const.value_as_text)
   end
   Print(indent.."Upvalues: "..proto_object.upv_qty)
   for j = 1, proto_object.upv_qty do
      local upv = proto_object.all_upvalues[j]
      Print(indent.."  "..rpad("Upv#"..j.." is parent function's "..(upv.in_locals and "R"..upv.index or "Upv#"..upv.index), 45)
         ..(upv.var_name and " name: "..upv.var_name or "")
      )
   end
   if not debug_info_is_stripped then
      Print(indent.."Locals: "..proto_object.locals_qty)
      for j = 1, proto_object.locals_qty do
         local loc = proto_object.all_locals[j]
         Print(indent.."  "..rpad("Local#"..j, 9).." R"..rpad(loc.reg_no, 3).." "
            ..rpad(j < proto_object.parameters_qty + (proto_object.has_local_arg and 2 or 1) and "parameter" or "def:<"..(loc.def_pc == 0 and "" or loc.def_pc)..">", 10)
            .." scope:<"..rpad((loc.start_pc > loc.end_pc and "" or loc.start_pc..(loc.start_pc == loc.end_pc and "" or ".."..loc.end_pc))..">", 12)
            .." name: "..loc.var_name
         )
      end
   end

   local function insert_consts_and_varnames(instr_as_text, pc, all_consts, all_locals, all_upvalues, arg_reg_no)
      local comments = {}
      instr_as_text = instr_as_text
         :gsub("^(.-)%s+(%-%-.*)$", "%1\0%2")
         :gsub("(@+)(%a*#?)(%d*)",
            function(dogs, word, num)
               local suffix = ""
               if word == "R" then
                  local num = assert(tonumber(num))
                  for _, loc in ipairs(all_locals) do
                     if loc.reg_no == num and pc >= (dogs == "@" and loc.start_pc or loc.def_pc) and pc <= loc.end_pc then
                        loc = loc.var_name
                        if loc:sub(1, 1) ~= "(" then
                           suffix = "@"..loc
                        end
                        break
                     end
                  end
                  if num == arg_reg_no then
                     suffix = "@arg"
                  end
               elseif word == "Const#" then
                  local const = assert(all_consts[tonumber(num)], "Const index is out of range")
                  local const_value_as_text = const.value_as_text
                  if #const_value_as_text < 50 then
                     table.insert(comments, word..num)
                     local const_type = const.type
                     if (const_type == "integer" or const_type == "float") and const_value_as_text:sub(1, 1) == "-" then
                        const_value_as_text = "("..const_value_as_text..")"
                     end
                     word, num = const_value_as_text, ""
                  end
               elseif word == "Upvalue#" then
                  word = "Upv#"
                  local upv = assert(all_upvalues[tonumber(num)], "Upvalue index is out of range").var_name
                  if upv then
                     suffix = "@"..upv
                  end
               else
                  error("Unrecognized symbol "..dogs..word..num)
               end
               return word..num..suffix
            end
         )
      local text, orig_comment = instr_as_text:match"^(%Z+)%z(%Z+)$"
      text = text or instr_as_text
      comments = (#comments == 0 and "" or "   -- It's "..table.concat(comments, ", "))..(orig_comment and "   "..orig_comment or "")
      return text..(comments == "" and "" or (" "):rep(45-#text:gsub("[\128-\191]", ""))..comments)
   end

   Print(indent.."Instructions: "..proto_object.instr_qty)
   for pc = 1, proto_object.instr_qty do
      local instr = proto_object.all_instructions[pc]
      Print(indent.."  "..instr.location_in_file.."   "
         ..(instr.src_line_no and rpad("line#"..instr.src_line_no, 9).." " or "")
         ..rpad(instr.instr_as_luac, 20).." "..lpad("<"..pc..">", 6).."   "
         ..insert_consts_and_varnames(instr.instr_as_text, pc, proto_object.all_consts, proto_object.all_locals, proto_object.all_upvalues, proto_object.has_local_arg and proto_object.parameters_qty)
      )
   end
   Print(indent.."Functions: "..proto_object.protos_qty)
   for j = 1, proto_object.protos_qty do
      print_proto_object(Print, proto_object.all_protos[j], Lua_version, depth + 1, debug_info_is_stripped, j, path)
   end
end

local function get_bytecode_listing(bytecode_object)
   local printed_lines = {}

   local function Print(line)
      table.insert(printed_lines, line or "")
   end

   Print("Lua version: "..bytecode_object.Lua_version_as_string)
   local enbi = bytecode_object.enbi
   local enbi_as_string =
      (enbi.is_LE and "L" or "B")
      ..(bytecode_object.Lua_version <= 0x53 and enbi.bytes_per_int..enbi.bytes_per_pointer or "")
      ..(enbi.bytes_per_lua_int or 0)
      ..(enbi.bytes_per_lua_float or 0)
   Print('EnBi = "'..enbi_as_string..'" (endianness and bitness of this bytecode)')
   Print("  Endianness          = "..(enbi.is_LE and "L (little-endian)" or "B (big-endian)"))
   if bytecode_object.Lua_version <= 0x53 then
      Print("  Size of C int       = "..enbi.bytes_per_int)
      Print("  Size of C pointer   = "..enbi.bytes_per_pointer)
   end
   Print("  Size of Lua integer = "..(enbi.bytes_per_lua_int or 0))
   Print("  Size of Lua float   = "..(enbi.bytes_per_lua_float or 0))
   Print("Debug info: "..(bytecode_object.debug_info_is_stripped and "stripped" or "included"))
   print_proto_object(Print, bytecode_object.root_proto, bytecode_object.Lua_version, 0, bytecode_object.debug_info_is_stripped)
   Print()
   return table.concat(printed_lines, "\n"), enbi_as_string
end

local function bcviewer(bytecode_as_string_or_loader, target_enbi)
   if target_enbi then
      -- converts bytecode to target EnBi and returns converted bytecode as binary string (or nil, err_mes)
      local ok, converted_bytecode = pcall(parse_or_convert_bytecode, bytecode_as_string_or_loader, target_enbi)
      if ok then
         return converted_bytecode
      else
         return nil, 'ERROR converting bytecode to EnBi "'..target_enbi..'"\n'..tostring(converted_bytecode)
      end
   else
      -- returns bytecode_listing, Lua_version_as_hex, EnBi_of_this_bytecode
      local ok, bytecode_listing, version, current_enbi = pcall(
         function ()
            local bytecode_object = parse_or_convert_bytecode(bytecode_as_string_or_loader)
            local listing, enbi = get_bytecode_listing(bytecode_object)
            return listing, bytecode_object.Lua_version, enbi
         end
      )
      if ok then
         return bytecode_listing, version, current_enbi
      else
         local err_text = "ERROR parsing bytecode\n"..tostring(bytecode_listing)
         return err_text
      end
   end
end

return bcviewer
