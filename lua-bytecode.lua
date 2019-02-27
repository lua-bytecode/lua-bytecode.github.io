-- lua-bytecode.lua

-- Version: 2019-02-27

local dot = assert(tostring(5.5):match"5(%p)5")
do
   local x = 2^53
   assert(x - (x-1) == 1 and (x+1) - x ~= 1, "Floating point numbers must be 'double'")
   local s = "-5"..dot.."5"
   assert(tostring(-5.5) == s and tonumber(s) == -5.5)
end

local function tohex(n, min_digits)
   -- this function is needed to workaround IE11-incompatibility of Fengari's string.format()
   local s = ("%X"):format(n)
   return ("0"):rep((min_digits or 1) - #s)..s
end

local function round_float8_to_float4(x)
   if not x or x == 0.0 or x ~= x then
      return x
   end
   local sign = 1.0
   if x < 0.0 then
      sign = -1.0
      x = -x
   end
   local min_pos_float4 = 2^-149
   local max_pos_float4 = 2^128 - 2^104
   if x > 2.0 * max_pos_float4 then
      return sign / 0.0
   elseif x < 0.5 * min_pos_float4 then
      return sign * 0.0
   end
   local e = 1.0
   if x >= e then
      while x >= e * 2.0 do
         e = e * 2.0
      end
   else
      repeat
         e = e * 0.5
      until x >= e
   end
   -- e <= x < 2*e
   local m = e * 2^-23
   x = x + 0.5 * m
   local r = x % m
   if r == 0.0 then
      r = x % (2.0 * m)
   end
   x = x - r
   if x > max_pos_float4 then
      return sign / 0.0
   elseif x < min_pos_float4 then
      return sign * 0.0
   else
      return sign * x
   end
end

local serialize_float_value, serialize_float_key
do
   local function increase_fixed_width_int(str)
      assert(str:match"^%d*$")
      for pos = #str, 1, -1 do
         local digit = str:sub(pos, pos)
         digit = digit == "9" and "0" or string.char(digit:byte() + 1)
         str = str:sub(1, pos - 1)..digit..str:sub(pos + 1)
         if digit ~= "0" then
            return str
         end
      end
      return str, true
   end

   local function string_to_float(str, bytes_per_float)
      str = str:gsub("^-?%d+$", "%0"..dot.."0")
      local x = tonumber(str)
      if bytes_per_float == 4 then
         return round_float8_to_float4(x)
      else
         return x
      end
   end

   local function float_to_compact_str(num, bytes_per_float)
      if bytes_per_float == 4 then
         num = round_float8_to_float4(num)
      end
      if num == 0.0 then
         return 1.0/num < 0.0 and "-0" or "0"
      elseif num ~= num then
         return "0/0"
      end
      local sign, x = "", num
      if x < 0.0 then
         sign = "-"
         x = -x
      end
      if x == 1.0 then
         return sign.."1"
      elseif x == 1/0 then
         return sign.."1/0"
      end
      local result, predicted_float8 = ("%.17g"):format(num)
      if bytes_per_float == 4 then
         local s = result:gsub("^-?%d+$", "%0"..dot.."0")
         predicted_float8 = tonumber(s)
      end
      do
         local prefix, inner, suffix = result:match("^(%-?)(%d*%"..dot.."%d*)(.*)$")
         if prefix then
            if suffix == "" then
               local e = assert(("%.17g"):format(1e33):match"[eE]")
               suffix = e.."0"
            end
            assert(suffix:match"^([eE])([-+]?%d+)$")
            repeat
               local finished, incr = true, inner:match"[0-4]$"
               for j = 1, 2 do
                  incr = not incr
                  if inner:match"%d$" then
                     local new_inner, new_suffix = inner:sub(1, -2), suffix
                     if incr then
                        local int, frac, carry = assert(new_inner:match("^(%d*)%"..dot.."(%d*)$"))
                        frac, carry = increase_fixed_width_int(frac)
                        if carry then
                           int = increase_fixed_width_int("0"..int):gsub("^0+", "")
                           if int:match"0$" then
                              local e, exp = assert(new_suffix:match"^([eE])([-+]?%d+)$")
                              int, frac = int:sub(1, -2), "0"..frac
                              new_suffix = e..tostring(tonumber(exp) + 1)
                           end
                        end
                        new_inner = int..dot..frac
                     end
                     if string_to_float(prefix..new_inner..new_suffix, bytes_per_float) == num then
                        inner, suffix, finished = new_inner, new_suffix
                        break
                     end
                  end
               end
            until finished
            result = prefix..inner:gsub("%"..dot.."$", "")..suffix
            if bytes_per_float == 4 then
               predicted_float8 = tonumber(result)  -- result is in exponential form here
            end
         end
         local mant, e, exp = result:match"^(.+)([eE])([-+]?%d+)$"
         if mant then
            -- convert XXe+YY to XXeYY
            exp = assert(tonumber(exp))
            result = exp == 0 and mant or mant..e..exp
         end
         assert(string_to_float(result, bytes_per_float) == num)
      end
      local e, k, found, N, D = 1.0, 0
      if x >= e then
         while x >= e * 2.0 do
            e = e * 2.0
            k = k + 1
         end
      else
         repeat
            e = e * 0.5
            k = k - 1
         until x >= e
      end
      -- e <= x < 2*e, e=2^k
      x = x / e
      local sane_width = bytes_per_float == 4 and 9 or 20
      local two_sw = 2^sane_width
      if x % (2.0 / two_sw) == 0.0 then
         k, found, N, D = k + 1 - sane_width, true, 0.5 * x * two_sw, 1.0
      else
         e, x = sign == "-" and -e or e, x - 1.0
         local pn, n, pd, d = 0.0, 1.0, 1.0, 0.0
         repeat
            local Q, q = x + 0.5, x - x % 1.0
            Q = Q - Q % 1.0
            pd, d, D = d, q*d + pd, Q*d + pd
            pn, n, N = n, q*n + pn, Q*n + pn + D
            local out_of_range = N > two_sw
            local appr = N/D * e
            if bytes_per_float == 4 then
               appr = round_float8_to_float4(appr)
            end
            found = not out_of_range and appr == num
            x = 1.0 / (x - q)
         until found or out_of_range or x ~= x or x > two_sw
      end
      if found then
         if k > 0 then
            while k > 0 and D % 2.0 == 0.0 do
               k, D = k - 1, D * 0.5
            end
            if k > 0 then
               if 2^k * N <= 10^6 then
                  k, N = 0, N * 2^k
               else
                  local max_degree_float = bytes_per_float == 4 and 127 or 1023
                  while k < max_degree_float and N % 2.0 == 0.0 do
                     k, N = k + 1, N * 0.5
                  end
               end
            end
         else
            while k < 0 and N % 2.0 == 0 do
               k, N = k + 1, N * 0.5
            end
            if k < 0 then
               if 2^-k * D <= 10^6 then
                  k, D = 0, D * 2^-k
               else
                  local min_degree_float = bytes_per_float == 4 and -149 or -1074
                  while k > min_degree_float and D % 2.0 == 0.0 do
                     k, D = k - 1, D * 0.5
                  end
               end
            end
         end
         local factor_exists = N ~= 1.0 or D ~= 1.0
         local power_exists = k ~= 0
         local result2 = sign
            ..(factor_exists and math.floor(N)..(D == 1.0 and "" or "/"..math.floor(D)) or "")
            ..(factor_exists and power_exists and " * " or "")
            ..(power_exists and "2^"..tostring(k) or "")
         if #result2 < #result then
            result = result2
            if bytes_per_float == 4 then
               predicted_float8 = (sign == "-" and -1.0 or 1.0) * (N / D) * 2^k
            end
         end
      end
      return result, predicted_float8
   end

   function serialize_float_value(x, bytes_per_float)
      -- appending ".0" to integer literals
      local str, predicted_float8 = float_to_compact_str(x, bytes_per_float)
      return str:gsub("^-?%d+$", "%0"..dot.."0"), predicted_float8
   end

   function serialize_float_key(x, bytes_per_float)
      -- [x]
      -- when float is used as a table key, there is no difference between "t[42]" and "t[42.0]", between "t[-0.0]", "t[0.0]" and "t[0]"
      -- decimal integer literals are allowed (instead of floats) when they are table keys
      local str, predicted_float8 = float_to_compact_str(x, bytes_per_float)
      return "["..(str == "-0" and "0" or str).."]", predicted_float8
   end

end

local serialize_string_value, serialize_string_key
do
   local escapings={["\a"]="\\a", ["\b"]="\\b", ["\t"]="\\t", ["\n"]="\\n", ["\v"]="\\v", ["\f"]="\\f", ["\r"]="\\r", ['"']='\\"', ["\\"]="\\\\"}

   function serialize_string_value(str)
      -- return '"'..str:gsub('[%c"\\]', function(c) return escapings[c] or ("\a%03d"):format(c:byte()) end):gsub("\a(%d%d%d%d)", "\\%1"):gsub("\a0?0?", "\\")..'"'
      return '"'..str:gsub('[%c"\\]', function(c) return escapings[c] or "\a"..tostring(1000 + c:byte()):sub(-3) end):gsub("\a(%d%d%d%d)", "\\%1"):gsub("\a0?0?", "\\")..'"'
   end

   local is_keyword = {
      ["and"]=0, ["break"]=0, ["do"]=0, ["else"]=0, ["elseif"]=0, ["end"]=0, ["false"]=0, ["for"]=0, ["function"]=0, ["goto"]=0, ["if"]=0,
      ["in"]=0, ["local"]=0, ["nil"]=0, ["not"]=0, ["or"]=0, ["repeat"]=0, ["return"]=0, ["then"]=0, ["true"]=0, ["until"]=0, ["while"]=0
   }

   function serialize_string_key(key)
      -- ["key"] or .key ?
      assert(type(key) == "string")
      return key:find"^[%a_][%w_]*$" and not is_keyword[key] and "."..key or "["..serialize_string_value(key).."]"
   end

end

local instr_modes = {
   ADD        = {"XA1KKiABC",   "", " + "},                --@  A B C   R(A) := RK(B) + RK(C)
   BAND       = {"XA1KKiABC",   "", " & "},                --@  A B C   R(A) := RK(B) & RK(C)
   BNOT       = {"XA2RNiAB",    "~ "},                     --@  A B     R(A) := ~R(B)
   BOR        = {"XA1KKiABC",   "", " | "},                --@  A B C   R(A) := RK(B) | RK(C)
   BXOR       = {"XA1KKiABC",   "", " ~ "},                --@  A B C   R(A) := RK(B) ~ RK(C)
   CALL       = {"XM_UUiABC"},                             --@  A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
   CLOSE      = {"X-3NNiA",     "CLOSE @R", ",.."},        --@  A       close all variables in the stack up to (>=) R(A)
   CLOSURE    = {"XA2UNiABx",   "Function#"},              --@  A Bx    R(A) := closure(KPROTO[Bx])
   CONCAT     = {"XA_RRiABC"},                             --@  A B C   R(A) := R(B).. ... ..R(C)
   DIV        = {"XA1KKiABC",   "", " / "},                --@  A B C   R(A) := RK(B) / RK(C)
   EQ         = {"V-_KKiABC"},                             --@  A B C   if ((RK(B) == RK(C)) ~= A) then pc++
   EXTRAARG   = {"X-3UUiAx",    "EXTRA_ARG "},             --@  Ax      extra (larger) argument for previous opcode
   FORLOOP    = {"X-_RNiAsBx"},                            --@  A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
   FORPREP    = {".M_RNiAsBx"},                            --@  A sBx   R(A)-=R(A+2); pc+=sBx
   GETGLOBAL  = {"XA2KNiABx",   "GLOBALS[", "]"},          --@  A Bx    R(A) := Gbl[Kst(Bx)]
   GETTABLE   = {"XA1RKiABC",   "", "[", "]"},             --@  A B C   R(A) := R(B)[RK(C)]
   GETTABUP   = {"XA1UKiABC",   "@Upvalue#", "[", "]"},    --@  A B C   R(A) := UpValue[B][RK(C)]
   GETUPVAL   = {"XA2UNiAB",    "@Upvalue#"},              --@  A B     R(A) := UpValue[B]
   IDIV       = {"XA1KKiABC",   "", " // "},               --@  A B C   R(A) := RK(B) // RK(C)
   JMP        = {".-_RNiAsBx"},                            --@  A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)
   JMP51      = {".-_RNisBx"},                             --@  sBx     pc+=sBx
   LE         = {"V-_KKiABC"},                             --@  A B C   if ((RK(B) <= RK(C)) ~= A) then pc++
   LEN        = {"XA2RNiAB",    "# "},                     --@  A B     R(A) := length of R(B)
   LOADBOOL   = {"CA_UUiABC"},                             --@  A B C   R(A) := (Bool)B; if (C) pc++
   LOADK      = {"XA2KNiABx"},                             --@  A Bx    R(A) := Kst(Bx)
   LOADKX     = {"XA2NNiA"},                               --@  A       R(A) := Kst(extra arg)
   LOADNIL    = {"XM_UNiAB"},                              --@  A B     R(A), R(A+1), ..., R(A+B) := nil
   LOADNIL51  = {"XM_RNiAB"},                              --@  A B     R(A) := ... := R(B) := nil
   LT         = {"V-_KKiABC"},                             --@  A B C   if ((RK(B) <  RK(C)) ~= A) then pc++
   MOD        = {"XA1KKiABC",   "", " % "},                --@  A B C   R(A) := RK(B) % RK(C)
   MOVE       = {"XA_RNiAB"},                              --@  A B     R(A) := R(B)
   MUL        = {"XA1KKiABC",   "", " * "},                --@  A B C   R(A) := RK(B) * RK(C)
   NEWTABLE   = {"XA_UUiABC"},                             --@  A B C   R(A) := {} (size = B,C)
   NOT        = {"XA2RNiAB",    "not "},                   --@  A B     R(A) := not R(B)
   POW        = {"XA1KKiABC",   "", " ^ "},                --@  A B C   R(A) := RK(B) ^ RK(C)
   RETURN     = {".-_UNiAB"},                              --@  A B     return R(A), ... ,R(A+B-2)
   SELF       = {"XM_RKiABC"},                             --@  A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]
   SETGLOBAL  = {"X-4KNiABx",   "GLOBALS[", "] = @R"},     --@  A Bx    Gbl[Kst(Bx)] := R(A)
   SETLIST    = {"L-_UUiABC"},                             --@  A B C   R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
   SETTABLE   = {"X-3KKiABC",   "@R", "[", "] = "},        --@  A B C   R(A)[RK(B)] := RK(C)
   SETTABUP   = {"X-3KKiABC",   "@Upvalue#", "[", "] = "}, --@  A B C   UpValue[A][RK(B)] := RK(C)
   SETUPVAL   = {"X-4UNiAB",    "@Upvalue#", " = @R"},     --@  A B     UpValue[B] := R(A)
   SHL        = {"XA1KKiABC",   "", " << "},               --@  A B C   R(A) := RK(B) << RK(C)
   SHR        = {"XA1KKiABC",   "", " >> "},               --@  A B C   R(A) := RK(B) >> RK(C)
   SUB        = {"XA1KKiABC",   "", " - "},                --@  A B C   R(A) := RK(B) - RK(C)
   TAILCALL   = {".-_UUiAB"},                              --@  A B     return R(A)(R(A+1), ... ,R(A+B-1))
   TEST       = {"V-_NUiAC"},                              --@  A C     if not (R(A) <=> C) then pc++
   TESTSET    = {"VA_RUiABC"},                             --@  A B C   if (R(B) <=> C) then R(A) := R(B) else pc++
   TFORCALL   = {"X-_NUiAC"},                              --@  A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));
   TFORLOOP   = {"XM_RNiAsBx"},                            --@  A sBx   if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }
   TFORLOOP51 = {"VM_NUiAC"},                              --@  A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
   UNM        = {"XA2RNiAB",    "- "},                     --@  A B     R(A) := -R(B)
   VARARG     = {"XM_UNiAB"},                              --@  A B     R(A), R(A+1), ..., R(A+B-2) = vararg
}

local function make_arg(B, B_mode, is_really_an_instruction)
   if B_mode == "K" and B < 256 or B_mode == "R" then
      assert(not is_really_an_instruction or B < 256, "Must be a register")
      return "@R"..B, B
   elseif B_mode == "K" then
      return "@Const#"..(B-255), 255-B
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

   local Lua_version
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

   local function write_int(value)
      write_double_as_unsigned_int(convert_to_enbi.bytes_per_int, value)
   end

   local function read_int()
      -- always mirror to target bytecode
      local value = read_unsigned_int_as_double(bytes_per_int)
      if convert_to_enbi then
         write_int(value)
      end
      return math.floor(value)
   end

   local function read_string()
      -- always mirror to target bytecode
      -- returns "string" or nil
      --    [size_t size_including_term_zero]
      --    string, 00
      local old_style = Lua_version <= 0x52
      local len_with_term_zero = old_style and 0xFF or read_byte(true)
      if len_with_term_zero == 0xFF then
         len_with_term_zero = read_unsigned_int_as_double(bytes_per_pointer)
         if convert_to_enbi then
            write_double_as_unsigned_int(convert_to_enbi.bytes_per_pointer, len_with_term_zero)
         end
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
            value, exp_bits, significand_bits = round_float8_to_float4(value), 8, 23
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
               assert(exp <= base_exp and exp >= low_exp - significand_bits)
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
         -- always mirror to target bytecode
         local LE_block, file_offset, data_in_file = read_LE_block(4, true)
         bits_in_buffer, str_in_LE_order, str_pos = 0, LE_block, 0
         -- Instruction format:
         --    31........................0
         --    B(9)..C(9)..A(8)..opcode(6)
         local opcode = math.floor(read_field(6))
         local A = math.floor(read_field(8))
         local C = math.floor(read_field(9))
         local B = math.floor(read_field(9))
         return opcode, A, B, C, file_offset, data_in_file
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

   assert(read_block(4, true) == "\27Lua", "Wrong bytecode signature.  Only vanilla Lua bytecodes are supported.")
   -- [u8 version] Version number (0x52 for Lua 5.2, etc)
   Lua_version = read_byte(true)
   assert(Lua_version >= 0x51 and Lua_version <= 0x53, "Lua version "..tohex(Lua_version, 2):gsub("^.", "%0.").." is not supported")

   -- [u8 impl] Implementation (0 = the bytecode is compatible with the "official" PUC-Rio implementation)
   assert(read_byte(true) == 0, "Bytecode is incompatible with PUC-Rio implementation")

   if convert_to_enbi then

      local function parse_target_enbi(enbi_as_string)
         local e, c_int, c_ptr, lua_int, lua_float = enbi_as_string:upper():match"^([LB])([48])([48])([048])([048])$"
         if not e then
            return "\nERROR: Wrong target EnBi string: "..enbi_as_string.."\nEnBi must match this regex: ^[LB][48][48][048][048]$"
         end
         c_int     = tonumber(c_int)
         c_ptr     = tonumber(c_ptr)
         lua_int   = tonumber(lua_int)
         lua_float = tonumber(lua_float)
         lua_int   = lua_int   ~= 0 and lua_int   or nil
         lua_float = lua_float ~= 0 and lua_float or nil
         if lua_int and lua_float then
            if Lua_version <= 0x52 then
               return "\nERROR: Wrong target EnBi string: "..enbi_as_string.."\nThis Lua version could have either Lua integer or Lua float, but not both"
            end
         elseif lua_int or lua_float then
            if Lua_version >= 0x53 then
               return "\nERROR: Wrong target EnBi string: "..enbi_as_string.."\nThis Lua version must have both Lua integer and Lua float"
            end
         else
            return "\nERROR: Wrong target EnBi string: "..enbi_as_string.."\nBoth Lua integer and Lua float are not specified"
         end
         return {
            is_LE               = e == "L",
            bytes_per_int       = c_int,
            bytes_per_pointer   = c_ptr,
            bytes_per_lua_int   = lua_int,
            bytes_per_lua_float = lua_float
         }
      end

      convert_to_enbi = parse_target_enbi(convert_to_enbi)
      if type(convert_to_enbi) == "string" then
         print("Lua version of the source bytecode: "..tohex(Lua_version, 2):gsub("^.", "%0."))
         error(convert_to_enbi)
      end
   end

   local instr_names = {}  -- [opcode] = "instruction name"
   do
      local all_instr = "0 MOVE LOADK 52+ LOADKX 0 LOADBOOL 51 LOADNIL51 52+ LOADNIL 0 GETUPVAL 51 GETGLOBAL 52+ GETTABUP 0 GETTABLE 51 SETGLOBAL 52+ SETTABUP 0 SETUPVAL SETTABLE NEWTABLE SELF ADD SUB MUL 52- DIV 0 MOD POW 53 DIV IDIV BAND BOR BXOR SHL SHR 0 UNM 53 BNOT 0 NOT LEN CONCAT 51 JMP51 52+ JMP 0 EQ LT LE TEST TESTSET CALL TAILCALL RETURN FORLOOP FORPREP 51 TFORLOOP51 52+ TFORCALL TFORLOOP 0 SETLIST 51 CLOSE 0 CLOSURE VARARG 52+ EXTRAARG"
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
                  assert(v1 or v3, "Wrong version mark "..word)
                  is_relevant = is_relevant
                     or v1 and Lua_version >= tonumber(v1, 16) and Lua_version <= tonumber(v2, 16)
                     or v3 and (
                        dir == "+" and Lua_version >= tonumber(v3, 16) or
                        dir == "-" and Lua_version <= tonumber(v3, 16) or
                        dir == "" and Lua_version == tonumber(v3, 16)
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
      -- 19 93 0D 0A 1A 0A   Lua magic (used to detect presence of EOL conversion)
      assert(read_block(6, true) == "\25\147\13\10\26\10", "Wrong file header")
   end

   if Lua_version >= 0x53 then
      read_sizes_of_int_ptr_instr_num()

      -- integer number check value
      local block = read_block(bytes_per_lua_int)
      if not read_lua_int(block:reverse()):equals(0x5678) then
         is_LE = true
         assert(read_lua_int(block):equals(0x5678), "Lua integer check failed")
      end
      if convert_to_enbi then
         write_lua_int(convert_double_to_lua_int_wo_mt(0x5678))
      end

      -- float number check value
      assert(read_lua_float() == 370.5, "Wrong FP number format")
      if convert_to_enbi then
         write_lua_float(370.5)
      end
   end

   local function get_extra_arg(next_instr)
      -- B(9)..C(9)..A(8)..opcode(6)
      local A, B, C, opcode, name = next_instr.A, next_instr.B, next_instr.C, next_instr.opcode, next_instr.name
      local Ax = (B * 512 + C) * 256 + A
      if Lua_version == 0x51 then
         Ax = Ax * 64 + opcode
      else
         assert(name == "EXTRAARG")
      end
      return Ax
   end

   local function parse_proto(is_root_proto)
      local all_upvalues, upv_qty, source, debug_info = {}  -- debug_info is a string: either "stripped" or "included"

      if Lua_version ~= 0x52 then
         if Lua_version >= 0x53 and is_root_proto then
            upv_qty = read_byte(true)
         end
         source = read_string()
         debug_info = source and "included" or "stripped"
      end

      -- [int line_start] debug info -- Line number in source code where chunk starts. 0 for the main chunk.
      local src_line_from = read_int()
      assert(is_root_proto or src_line_from > 0)
      -- [int line_end] debug info -- Line number in source code where chunk stops. 0 for the main chunk.
      local src_line_to = read_int()
      assert(src_line_to >= src_line_from)

      if Lua_version == 0x51 then
         upv_qty = read_byte(true)
         for j = 1, upv_qty do
            all_upvalues[j] = {}
         end
      end

      -- [u8 nparams] -- Number of parameters
      local arguments_qty = read_byte(true)
      if src_line_from == 0 then  -- for main chunk
         assert(arguments_qty == 0)
      end
      -- [u8 varargflags] -- vararg flag
      local is_vararg, has_local_arg, local_arg_contains_table
      if Lua_version == 0x51 then
         local n = read_byte(true)
         assert(n < 8, "Wrong vararg flags")
         if src_line_from == 0 then  -- for main chunk
            assert(n == 2)
         end
         is_vararg = n % 4 > 1
         has_local_arg = n % 2 > 0
         local_arg_contains_table = n > 3
      else
         is_vararg = read_boolean(true)
      end

      -- [u8 nregisters] -- number of registers used by this function
      local registers_needed = read_byte(true)

      -- Instructions
      local instr_qty = read_int()
      assert(instr_qty > 0)
      local all_instructions = {}  -- [1..n] = {opcode=, A=, B=, C=, name=, location_in_file="...", src_line_no=, instr_as_text=, instr_as_luac=, is_reachable=}
      -- [int ninstructions]
      for j = 1, instr_qty do
         -- [instsize instruction]
         local opcode, A, B, C, file_offset, data_in_file = read_instruction()
         local instr_name = instr_names[opcode]
         --local location_in_file = ("+%04X: %02X %02X %02X %02X"):format(file_offset, data_in_file:byte(1, 4))
         local location_in_file = "+"..tohex(file_offset, 4)..": "..tohex(data_in_file:byte(), 2).." "..tohex(data_in_file:byte(2), 2).." "..tohex(data_in_file:byte(3), 2).." "..tohex(data_in_file:byte(4), 2)
         all_instructions[j] = {opcode = opcode, A = A, B = B, C = C, name = instr_name, location_in_file = location_in_file}
      end

      -- Determine which instructions are reachable
      all_instructions[1].is_reachable = true
      do
         local fresh_pc_list = {1}
         repeat
            local new_fresh_pc_list = {}
            for _, pc in ipairs(fresh_pc_list) do
               local instr = all_instructions[pc]
               local name, deltas = instr.name
               if name then
                  local m = instr_modes[name][1]
                  deltas = {}
                  if m:match"sBx$" then
                     local sBx = instr.B * 512 + instr.C - 131071
                     table.insert(deltas, sBx)  -- pc+1+sBx
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

      -- [int nconsts]
      local const_qty = read_int()
      local all_consts = {}  -- [0..(n-1)] = {type="nil/boolean/string/float/integer", value=, value_as_text=, location_in_file=}
      for j = 1, const_qty do
         -- [u8 type]
         local const_type_id = read_byte(true)
         local const_type, const_value, const_value_as_text, file_offset, data_in_file
         if const_type_id == 4 then
            -- type 4:  string
            const_type = "string"
            const_value, file_offset = assert(read_string())
            data_in_file = const_value
            assert(const_value, "String is absent")
            const_value_as_text = serialize_string_value(const_value)
         elseif const_type_id == 3 or const_type_id == 3+16 then
            if const_type_id == 3 and bytes_per_lua_float then
               -- type 3:  number(5.2-), float(5.3+)
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
               -- type 3+16:  integer(5.3+)
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
         --local location_in_file = ("+%04X:Size=%X:"):format(file_offset, #data_in_file)
         local location_in_file = "+"..tohex(file_offset, 4)..":Size="..tohex(#data_in_file)..":"
         for j = 1, math.min(8, #data_in_file) do
            location_in_file = location_in_file.." "..tohex(data_in_file:byte(j), 2)
         end
         local target_length = #"+FFFF:Size=8: 01 02 03 04 05 06 07 08"
         if #data_in_file > 8 or #location_in_file > target_length then
            location_in_file = location_in_file:sub(1, target_length - 3).."..."
         end
         location_in_file = location_in_file..(" "):rep(target_length - #location_in_file)
         all_consts[j] = {type = const_type, value = const_value, value_as_text = const_value_as_text, location_in_file = location_in_file}
      end

      local function parse_upvalues(qty)
         -- [int nupvals]
         upv_qty = read_int()
         assert(upv_qty == (qty or upv_qty))
         for j = 1, upv_qty do
            -- [u8 stack] -- 1 = in registers of enclosing function, 0 = in upvalues of enclosing function
            local in_locals = read_boolean(true)
            -- [u8 register] -- idx (either idx of register or idx of upvalue)
            local index = read_byte(true)
            all_upvalues[j] = {in_locals = in_locals, index = in_locals and index or index + 1}    -- [1..n] = {in_locals=true/false, index=, var_name=}
         end
      end

      if Lua_version >= 0x53 then   -- starting with Lua 5.3, Upvalues and Prototypes were swapped in the bytecode layout
         parse_upvalues(upv_qty)
      end

      -- [int nfunctions]
      local protos_qty = read_int()
      local all_protos = {}
      for j = 1, protos_qty do
         local proto, debug_info_in_proto = parse_proto()
         all_protos[j] = proto
         debug_info = debug_info or debug_info_in_proto
         assert(debug_info == debug_info_in_proto)
      end

      if Lua_version == 0x52 then
         parse_upvalues()
         -- [string source] | debug info
         source = read_string()
         local new_debug_info = source and "included" or "stripped"
         debug_info = debug_info or new_debug_info
         assert(debug_info == new_debug_info)
      end

      -- at this point we know for sure whether this bytecode has debug info or not
      assert(debug_info == "stripped" or debug_info == "included")

      -- [int nlines]
      local lines_qty = read_int()
      assert(
         debug_info == "stripped" and lines_qty == 0 or
         debug_info == "included" and lines_qty == instr_qty
      )
      for j = 1, lines_qty do
         -- [int line]
         all_instructions[j].src_line_no = read_int()
      end

      -- [int nlocals]
      local locals_qty = read_int()
      assert(
         debug_info == "stripped" and locals_qty == 0 or
         debug_info == "included" and locals_qty >= arguments_qty + (has_local_arg and 1 or 0)
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
         debug_info == "stripped" and upval_names_qty == 0 or
         debug_info == "included" and upval_names_qty == upv_qty
      )
      for j = 1, upval_names_qty do
         -- [string name]  debug info
         all_upvalues[j].var_name = assert(read_string())
      end
      if src_line_from == 0 and Lua_version >= 0x52 and upv_qty == 1 and not all_upvalues[1].var_name then
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

         -- propagate _ENV upvalue name recursively down to child prototypes
         set_name_for_the_ENV_upvalue(all_protos, 1)
      end

      -- Disassembling (preparing fields instr_as_luac and instr_as_text) without resolving upvalue, local, const names
      local data_items_ahead, data_descr = 0
      local last_closure_51_pc, last_closure_51_upvalues, pc_after_TFORLOOP51
      for pc = 1, instr_qty do
         local instr = all_instructions[pc]
         local A, B, C, name, is_reachable = instr.A, instr.B, instr.C, instr.name, instr.is_reachable
         local Bx = B * 512 + C
         local sBx = Bx - 131071
         local Ax = Bx * 256 + A
         local instr_as_luac, instr_as_text
         local is_really_an_instruction = data_items_ahead == 0
         if name then
            local mode = assert(instr_modes[name])
            local data_1, data_2, data_3, data_4 = mode[2] or "", mode[3] or "", mode[4] or "", mode[5] or ""
            local instr_writes_to = mode[1]:sub(2, 2)
            local group           = mode[1]:sub(3, 3)
            local B_mode          = mode[1]:sub(4, 4)
            local C_mode          = mode[1]:sub(5, 5)
            mode = mode[1]:sub(6)
            local params, params_luac
            if mode == "iABC" then
               local B_arg, B_arg_luac = make_arg(B, B_mode, is_really_an_instruction)
               local C_arg, C_arg_luac = make_arg(C, C_mode, is_really_an_instruction)
               params = {A, B_arg, C_arg}
               params_luac = {A, B_arg_luac, C_arg_luac}
            elseif mode == "iAB" then
               local B_arg, B_arg_luac = make_arg(B, B_mode, is_really_an_instruction)
               params = {A, B_arg}
               params_luac = {A, B_arg_luac}
            elseif mode == "iAC" then
               local C_arg, C_arg_luac = make_arg(C, C_mode, is_really_an_instruction)
               params = {A, C_arg}
               params_luac = {A, C_arg_luac}
            elseif mode == "iA" then
               params = {A}
               params_luac = params
            elseif mode == "iABx" then
               if is_really_an_instruction then
                  assert(B_mode == "U" or B_mode == "K")
                  params = {A, B_mode == "K" and "@Const#"..(Bx + 1) or Bx}
               end
               params_luac = {A, B_mode == "K" and -1-Bx or Bx}
            elseif mode == "iAsBx" then
               if is_really_an_instruction then
                  assert(B_mode == "R")
                  params = {A, "<"..(pc + 1 + sBx)..">"}
               end
               params_luac = {A, sBx}
            elseif mode == "isBx" then
               if is_really_an_instruction then
                  assert(B_mode == "R")
                  params = {"<"..(pc + 1 + sBx)..">"}
               end
               params_luac = {sBx}
            elseif mode == "iAx" then
               params = {Ax}
               params_luac = params
            else
               error"Wrong instruction mode"
            end
            instr_as_luac = name:gsub("51$", "").." "..table.concat(params_luac, " ")
            if is_reachable then
               if is_really_an_instruction then
                  if group == "1" then  -- binary operators
                     assert(mode == "iABC")
                     if name == "GETTABUP" then
                        params[2] = B + 1
                     end
                     instr_as_text = "@@R"..params[1].." = "..data_1..params[2]..data_2..params[3]..data_3
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
                        end
                     end
                     instr_as_text = "@@R"..params[1].." = "..data_1..params[2]..data_2
                  elseif group == "3" or group == "4" then
                     if name == "SETTABUP" then
                        params[1] = A + 1
                     elseif name == "SETUPVAL" then
                        params[2] = B + 1
                     end
                     local param1, param2 = params[1], params[2]
                     if group == "4" then
                        param1, param2 = param2, param1
                     end
                     instr_as_text = data_1..param1..data_2..(param2 or "")..data_3..(params[3] or "")..data_4
                  elseif name == "MOVE" then
                     -- A B     R(A) := R(B)
                     if A == B then
                        instr_as_text = "NOP"
                     else
                        instr_as_text = "@@R"..A.." = @R"..B
                     end
                  elseif name == "JMP51" or name == "JMP" then
                     -- JMP    A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)
                     -- JMP51  sBx     pc+=sBx
                     local close_needed = name == "JMP" and A > 0
                     local goto_needed = sBx ~= 0
                     instr_as_text =
                        (close_needed or goto_needed)
                        and
                           (close_needed and "CLOSE @R"..(A-1)..",.." or "")
                           ..(close_needed and goto_needed and ";  " or "")
                           ..(goto_needed and "GOTO "..params[#params] or "")
                        or
                           "NOP"
                     if pc == pc_after_TFORLOOP51 then
                        instr_as_text = instr_as_text.."  -- end loop"
                        local loop_start_instr = all_instructions[pc + sBx]
                        local prev_instr = all_instructions[pc - 1]
                        local A = prev_instr.A
                        local C = prev_instr.C
                        loop_start_instr.instr_as_text = loop_start_instr.instr_as_text.."   -- for "..regs(A+3, A+2+C, true).." in @R"..A..", @R"..(A+1)..", @R"..(A+2).." do"
                     end
                  elseif name == "TFORCALL" then
                     -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
                     assert(C > 0)
                     assert(all_instructions[pc + 1].name == "TFORLOOP")
                     instr_as_text = regs(A+3, A+2+C, true).." = @R"..A.."(@R"..(A+1)..", @R"..(A+2)..")"
                  elseif name == "TFORLOOP" then
                     -- A sBx   if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }
                     instr_as_text = "if @R"..(A+1).." ~= nil then { @@R"..A.." = @R"..(A+1)..";  GOTO "..params[2].." }  -- end loop"
                     do
                        local loop_start_instr = all_instructions[pc + sBx]
                        local prev_instr = all_instructions[pc - 1]
                        assert(prev_instr.name == "TFORCALL")
                        local A = A - 2
                        assert(A == prev_instr.A)
                        local C = prev_instr.C
                        loop_start_instr.instr_as_text = loop_start_instr.instr_as_text.."   -- for "..regs(A+3, A+2+C, true).." in @R"..A..", @R"..(A+1)..", @R"..(A+2).." do"
                     end
                  elseif name == "TFORLOOP51" then
                     -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
                     assert(C > 0)
                     assert(all_instructions[pc + 1].name == "JMP51")
                     instr_as_text = regs(A+3, A+2+C, true).." = @R"..A.."(@R"..(A+1)..", @R"..(A+2)..");  if @R"..(A+3).." ~= nil then @@R"..(A+2).." = @R"..(A+3).." else GOTO <"..(pc + 2)..">"
                     pc_after_TFORLOOP51 = pc + 1
                  elseif name == "LOADNIL" or name == "LOADNIL51" then
                     -- LOADNIL    A B     R(A), R(A+1), ..., R(A+B) := nil
                     -- LOADNIL51  A B     R(A) := ... := R(B) := nil
                     instr_as_text = regs(A, name == "LOADNIL" and A + B or B, true).." = nil"
                  elseif name == "SELF" then
                     -- A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]
                     instr_as_text = "@@R"..(A+1).." = @R"..B..";  @@R"..A.." = @R"..B.."["..params[3].."]   -- prepare for @R"..B..":method()"
                  elseif name == "FORLOOP" then
                     -- A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
                     instr_as_text = "@@R"..A.." += @R"..(A+2)..";  if @R"..A.." <?= @R"..(A+1).." then { @@R"..(A+3).." = @R"..A..";  GOTO "..params[2].." }  -- end loop"
                  elseif name == "FORPREP" then
                     -- A sBx   R(A)-=R(A+2); pc+=sBx
                     instr_as_text = "@@R"..A.." -= @R"..(A+2)..";  GOTO "..params[2].."  -- for @@R"..(A+3).." = @R"..A..", @R"..(A+1)..", @R"..(A+2).." do"
                  elseif name == "TESTSET" then
                     -- A B C   if (R(B) <=> C) then R(A) := R(B) else pc++
                     assert(C < 2)
                     instr_as_text = "if "..(C == 0 and "not " or "").."@R"..B.." then @@R"..A.." = @R"..B.." else GOTO <"..(pc + 2)..">"
                  elseif name == "TEST" then
                     -- A C     if not (R(A) <=> C) then pc++
                     assert(C < 2)
                     instr_as_text = "if "..(C > 0 and "not " or "").."@R"..A.." then GOTO <"..(pc + 2)..">"
                  elseif name == "LE" or name == "LT" then
                     -- LE   A B C   if ((RK(B) <= RK(C)) ~= A) then pc++
                     -- LT   A B C   if ((RK(B) <  RK(C)) ~= A) then pc++
                     assert(A < 2)
                     local operation = ({LE = " <= ", LT = " < "})[name]
                     local left, right = params[2], params[3]
                     if B > 255 and C <= 255 then
                        left, right = right, left
                        operation = operation:gsub(".", {[">"] = "<", ["<"] = ">"})
                     end
                     instr_as_text = "if "..(A > 0 and "not (" or "")..left..operation..right..(A > 0 and ")" or "").." then GOTO <"..(pc + 2)..">"
                  elseif name == "EQ" then
                     -- A B C   if ((RK(B) == RK(C)) ~= A) then pc++
                     assert(A < 2)
                     local left, right = params[2], params[3]
                     if B > 255 and C <= 255 then
                        left, right = right, left
                     end
                     instr_as_text = "if "..left..(A == 0 and " == " or " ~= ")..right.." then GOTO <"..(pc + 2)..">"
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
                     instr_as_text = "@R"..A.."["..from_idx.."]"..(
                        B == 0 and ",.."
                        or B == 1 and ""
                        or B == 2 and ", @R"..A.."["..(from_idx + 1).."]"
                        or B == 3 and ", @R"..A.."["..(from_idx + 1).."]"..", @R"..A.."["..(from_idx + 2).."]"
                        or ",..,@R"..A.."["..(from_idx + B - 1).."]"
                     ).." = "..regs(A+1, B>0 and A+B)
                  elseif name == "CONCAT" then
                     local t = {}
                     for j = B, C do
                        t[#t + 1] = "@R"..j
                     end
                     instr_as_text = "@@R"..A.." = "..table.concat(t, "..")
                  elseif name == "CALL" then
                     --  A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
                     --     (*)  If (B == 0) then B = top.
                     --          If (C == 0) then 'top' is set to last_result+1, so next open instruction (CALL, RETURN, SETLIST) may use 'top'.
                     instr_as_text = (C == 1 and "" or regs(A, C>0 and A+C-2, true).." = ").."@R"..A.."("..(B == 1 and "" or regs(A+1, B>0 and A+B-1))..")"
                  elseif name == "TAILCALL" then
                     --@  A B     return R(A)(R(A+1), ... ,R(A+B-1))
                     instr_as_text = "RETURN @R"..A.."("..(B == 1 and "" or regs(A+1, B>0 and A+B-1))..")"
                  elseif name == "RETURN" then
                     --@  A B     return R(A), ... ,R(A+B-2)
                     --    (*) If (B == 0) then return up to 'top'.
                     instr_as_text = "RETURN"..(B == 1 and "" or " "..regs(A, B>0 and A+B-2))
                  elseif name == "VARARG" then
                     --@  A B     R(A), R(A+1), ..., R(A+B-2) = vararg
                     --   (*) If (B == 0) then use actual number of varargs and set top (like in CALL with C == 0).
                     instr_as_text = B == 1 and "NOP" or (regs(A, B>0 and A+B-2, true).." = ...")
                  elseif name == "LOADBOOL" then
                     --@  A B C   R(A) := (Bool)B; if (C) pc++
                     assert(B < 2 and C < 2)
                     instr_as_text = "@@R"..A.." = "..tostring(B > 0)..(C > 0 and ";  GOTO <"..(pc + 2)..">" or "")
                  else
                     error("There is no disassembly code for instruction "..name)
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
                        elseif name == "CALL" then
                           -- CALL       -- A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
                           regs_to = math.huge
                        elseif name == "VARARG" then
                           -- VARARG     -- A B     R(A), R(A+1), ..., R(A+B-2) = vararg
                           regs_to = A + B - 2
                        elseif name == "FORPREP" then
                           -- FORPREP    -- A sBx   R(A)-=R(A+2); pc+=sBx
                           -- (loop body)
                           -- FORLOOP    -- A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
                           regs_from = A + 3
                           regs_to = regs_from
                        elseif name == "TFORLOOP" then
                           -- JMP
                           -- (loop body)
                           -- TFORCALL   -- A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));
                           -- TFORLOOP   -- A sBx   if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }
                           assignment_pc, regs_from, regs_to = pc + sBx, A + 1, A + all_instructions[pc - 1].C
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
                        if loc_reg >= regs_from and loc_reg <= regs_to and assignment_pc < loc.start_pc and assignment_pc > loc.def_pc then
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
                     upv_info.index = B + 1
                  end
               end
            end
         else
            instr_as_luac = instr.opcode.." "..A.." "..B.." "..C
            if is_really_an_instruction then
               instr_as_text = "UNRECOGNIZED opcode="..instr.opcode..", A="..A..", B="..B..", C="..C
            end
         end
         if not is_reachable then
            instr_as_text = "(unreachable instruction)"
         elseif not is_really_an_instruction then
            data_items_ahead = data_items_ahead - 1
            instr_as_text = "(data for previous instruction)"
            if data_descr == "extra_arg_51" then
               instr_as_luac = ("%.0f"):format(Ax * 64.0 + instr.opcode)
            end
         end
         instr.instr_as_luac = instr_as_luac
         instr.instr_as_text = instr_as_text
      end

      return {   -- proto object
         source = source,
         src_line_from = src_line_from,
         src_line_to = src_line_to,
         arguments_qty = arguments_qty,
         is_vararg = is_vararg,
         has_local_arg = has_local_arg,
         local_arg_contains_table = local_arg_contains_table,
         registers_needed = registers_needed,
         const_qty = const_qty,
         all_consts = all_consts,
         upv_qty = upv_qty,
         all_upvalues = all_upvalues,
         locals_qty = locals_qty,
         all_locals = all_locals,
         instr_qty = instr_qty,
         all_instructions = all_instructions,
         protos_qty = protos_qty,
         all_protos = all_protos,
      },
      debug_info
   end

   local root_proto, debug_info = parse_proto(true)

   return convert_to_enbi and table.concat(target_bytecode) or {
      -- bytecode object
      Lua_version = Lua_version,
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


local function print_proto_object(Print, proto_object, Lua_version, depth, subfunc_idx, path)
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
      Print(indent
         .."Source line#: "..proto_object.src_line_from
         ..(proto_object.src_line_from == proto_object.src_line_to and "" or ".."..proto_object.src_line_to)
      )
   end
   Print(indent.."Arguments: "..proto_object.arguments_qty)
   Print(indent.."Is vararg: "..(proto_object.is_vararg and "yes" or "no"))
   if proto_object.is_vararg and Lua_version == 0x51 then
      Print(indent.."Local arg: "..(
         proto_object.has_local_arg
            and (proto_object.local_arg_contains_table and "contains array of arguments" or "contains nil")
            or "absent"
      ))
   end
   Print(indent.."VM registers needed: "..proto_object.registers_needed)
   Print(indent.."Constants: "..proto_object.const_qty)
   for j = 1, proto_object.const_qty do
      Print(indent.."  "
         ..proto_object.all_consts[j].location_in_file
         ..rpad("   Const#"..j, 12)
         .." "..rpad(proto_object.all_consts[j].type, 9)
         .." "..proto_object.all_consts[j].value_as_text
      )
   end
   Print(indent.."Upvalues: "..proto_object.upv_qty)
   for j = 1, proto_object.upv_qty do
      Print(indent.."  "
         ..rpad("U"..j, 3).." is enclosing function's "
         ..rpad(proto_object.all_upvalues[j].in_locals
            and "R"..proto_object.all_upvalues[j].index
            or "Upvalue#"..proto_object.all_upvalues[j].index, 11)
         .." "..(proto_object.all_upvalues[j].var_name and "name: "..proto_object.all_upvalues[j].var_name or "")
      )
   end
   if proto_object.locals_qty ~= 0 then
      Print(indent.."Locals: "..proto_object.locals_qty)
      for j = 1, proto_object.locals_qty do
         Print(indent.."  "
            ..rpad("Local#"..j, 9)
            .." R"..rpad(proto_object.all_locals[j].reg_no, 3)
            .." pc:<"
            ..rpad(
               proto_object.all_locals[j].def_pc..";"..(
                  proto_object.all_locals[j].start_pc > proto_object.all_locals[j].end_pc
                  and
                     ""
                  or
                     proto_object.all_locals[j].start_pc..(
                        proto_object.all_locals[j].start_pc == proto_object.all_locals[j].end_pc
                        and ""
                        or ".."..proto_object.all_locals[j].end_pc
                     )
               )..">", 14)
            .." name: "..proto_object.all_locals[j].var_name
         )
      end
   end

   local function insert_consts_and_varnames(instr_as_text, pc, all_consts, all_locals, all_upvalues, arg_reg_no)
      local comments = {}
      instr_as_text = instr_as_text
         :gsub("^(.-)%s*(%-%-.*)$", "%1\0%2")
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
                  local const = assert(all_consts[tonumber(num)]).value_as_text
                  if #const < 50 then
                     table.insert(comments, word..num)
                     word, num = const, ""
                  end
               elseif word == "Upvalue#" then
                  word = "U"
                  local upv = assert(all_upvalues[tonumber(num)]).var_name
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
      return text..(comments == "" and "" or (" "):rep(28-#text:gsub("[\128-\191]", ""))..comments)
   end

   Print(indent.."Instructions: "..proto_object.instr_qty)
   for pc = 1, proto_object.instr_qty do
      local instr = proto_object.all_instructions[pc]
      Print(indent.."  "..instr.location_in_file.."   "
         ..(instr.src_line_no and rpad("line#"..instr.src_line_no, 9).." " or "")
         ..rpad(instr.instr_as_luac, 20).." "..lpad("<"..pc..">", 6).."   "
         ..insert_consts_and_varnames(instr.instr_as_text, pc, proto_object.all_consts, proto_object.all_locals, proto_object.all_upvalues, proto_object.has_local_arg and proto_object.arguments_qty)
      )
   end
   Print(indent.."Functions: "..proto_object.protos_qty)
   for j = 1, proto_object.protos_qty do
      print_proto_object(Print, proto_object.all_protos[j], Lua_version, depth + 1, j, path)
   end
end

local function get_bytecode_listing(bytecode_object)
   local printed_lines = {}

   local function Print(line)
      table.insert(printed_lines, line or "")
   end

   Print("Lua version: "..tohex(bytecode_object.Lua_version, 2):gsub("^.", "%0."))
   local enbi = bytecode_object.enbi
   local enbi_as_string =
      (enbi.is_LE and "L" or "B")
      ..enbi.bytes_per_int
      ..enbi.bytes_per_pointer
      ..(enbi.bytes_per_lua_int or 0)
      ..(enbi.bytes_per_lua_float or 0)
   Print('EnBi = "'..enbi_as_string..'" (endianness and bitness of this bytecode)')
   Print("  Endianness          = "..(enbi.is_LE and "L (little-endian)" or "B (big-endian)"))
   Print("  Size of C int       = "..enbi.bytes_per_int)
   Print("  Size of C pointer   = "..enbi.bytes_per_pointer)
   Print("  Size of Lua integer = "..(enbi.bytes_per_lua_int or 0))
   Print("  Size of Lua float   = "..(enbi.bytes_per_lua_float or 0))
   Print("Debug info: "..(bytecode_object.debug_info_is_stripped and "stripped" or "included"))
   print_proto_object(Print, bytecode_object.root_proto, bytecode_object.Lua_version, 0)
   Print()
   return table.concat(printed_lines, "\n"), enbi_as_string
end

local function get_decompiled_source(bytecode_object)
   return "Not implemented yet"
end

local function bcviewer(bytecode_as_string_or_loader, target_enbi)
   if target_enbi then
      -- convert bytecode to target EnBi and return converted bytecode as binary string
      return parse_or_convert_bytecode(bytecode_as_string_or_loader, target_enbi)
   else
      -- return bytecode listing and decompiled source
      local ok, bytecode_listing, decompiled_source, version, current_enbi = pcall(
         function ()
            local bytecode_object = parse_or_convert_bytecode(bytecode_as_string_or_loader)
            local listing, enbi = get_bytecode_listing(bytecode_object)
            local decomp = get_decompiled_source(bytecode_object)
            return listing, decomp, bytecode_object.Lua_version, enbi
         end
      )
      if ok then
         return bytecode_listing, decompiled_source, version, current_enbi
      else
         local err_text = "ERROR parsing bytecode\n"..tostring(bytecode_listing)
         return err_text, err_text
      end
   end
end

return bcviewer
