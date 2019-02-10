{-# LANGUAGE OverloadedStrings #-}

module LuauVM.VM where

import Data.Vector(Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Maybe
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Language.Lua.Bytecode
import Language.Lua.Bytecode.Parser

-- Config constants
minStack          = 20
maxStack          = 1000000
maxCallCount      = 200
errorStackSize    = maxStack + 200
extraStack        = 5
basicStackSize    = 2 * minStack
maxTagLoop        = 100
firstPseudoIndex  = -maxStack - 1000
maxUpValue        = 256 -- math.MaxUint8
idSize            = 60
apiCheck          = False
internalCheck     = False
pathListSeparator = ';'

upValueIndex :: Int -> Int
upValueIndex i = firstPseudoIndex - i

isPseudoIndex :: Int -> Bool
isPseudoIndex i = i <= firstPseudoIndex

data Value = LNil |
             LBoolean Bool |
             LNumber Double |
             LInt Int |
             LString ByteString |
             LFunction | -- Not sure what this one should look like!
             HFunction (LState -> IO (LState, [Value])) |
             LUserdata ByteString |
             LThread |
             LTable

instance Show Value where
  show (HFunction _) = "HFunction (LState -> IO (LState, [Value]))"
  show LNil = "nil"
  show (LBoolean b) = show b
  show (LNumber d) = show d
  show (LInt i) = show i
  show (LString bs) = show bs
  show LFunction  = "LFunction"
  show (LUserdata bs) = show bs
  show LThread = "LThread"
  show LTable = "LTable"

constToValue :: Constant -> Value
constToValue KNil = LNil
constToValue (KBool b) = LBoolean b
constToValue (KNum x) = LNumber x
constToValue (KInt x) = LInt x
constToValue (KString x) = LString x
constToValue (KLongString x) = LString x

data ArithOp =  LUA_OPADD | -- performs addition (+)
                LUA_OPSUB | -- performs subtraction (-)
                LUA_OPMUL | -- performs multiplication (*)
                LUA_OPDIV | -- performs float division (/)
                LUA_OPIDIV | -- performs floor division (//)
                LUA_OPMOD | -- performs modulo (%)
                LUA_OPPOW | -- performs exponentiation (^)
                LUA_OPUNM | -- performs mathematical negation (unary -)
                LUA_OPBNOT | -- performs bitwise NOT (~)
                LUA_OPBAND | -- performs bitwise AND (&)
                LUA_OPBOR | -- performs bitwise OR (|)
                LUA_OPBXOR | -- performs bitwise exclusive OR (~)
                LUA_OPSHL | -- performs left shift (<<)
                LUA_OPSHR -- performs right shift (>>)
              deriving (Show, Eq)

data CallInfo = CallInfo {
  funcIdx :: Int, -- Index of function in the stack
  topIdx :: Int, -- Index of top of stack for the function
  previous :: Maybe CallInfo,
  next :: Maybe CallInfo
} deriving (Show)

newCallInfo = CallInfo 0 0 Nothing Nothing

data LState = LState { -- Technically thread state, make global state later
  stack :: Seq Value,
  stackTop :: Int, -- first free slot in the slack
  stackLast :: Int, -- last free slot in the slack (do I need this?)
  constants :: Vector Constant,
  program :: Vector OpCode,
  upValues :: [Map ByteString Value],
  callInfo :: CallInfo,
  registry :: Map ByteString Value -- TODO: Move to global state
} deriving (Show)

push :: LState -> Value -> LState
push s v = s { stack = newStack, stackTop = top + 1}
  where top = stackTop s
        newStack = set top v (stack s)

pop :: LState -> (LState, Value)
pop s = (s { stackTop = top - 1 }, fromJust $ theStack S.!? (top - 1))
  where top = stackTop s
        theStack = stack s

-- REPLICATE C API?
--void lua_arith (lua_State *L, int op)
arith :: LState -> ArithOp -> LState
arith s op = undefined

lua_gettop :: LState -> Int
lua_gettop s = top - (func + 1)
  where top = (stackTop s)
        func = (funcIdx $ callInfo s)

lua_setglobal :: LState -> String -> LState
lua_setglobal s name = undefined

lua_getglobal :: LState -> ByteString -> LState
lua_getglobal s name = newState { stack = set newTop theFn (stack s)}
  where globalTable = registry s
        theFn = globalTable M.! name
        newState = push s (LString name)
        newTop = stackTop newState

  -- LUA_API int lua_getglobal (lua_State *L, const char *name) {
  --   Table *reg = hvalue(&G(L)->l_registry);
  --   lua_lock(L);
  --   return auxgetstr(L, luaH_getint(reg, LUA_RIDX_GLOBALS), name);
  -- }

-- insert :: State -> Int -> State -- inserts element into index, moves other elements
-- insert s index = s { stack = h ++ [val] ++ t }
--   where val = (stack s) !! index
--         (h, t) = splitAt index (stack s)
--
-- delete :: State -> Int -> State -- deletes element from stack, moves other elements
-- delete s index = s { stack = h ++ (tail t) }
--   where (h, t) = splitAt index (stack s)
--
-- call :: State -> Int -> Int -> IO (State) -- Maybe shouldn't have to be IO but whatever, some are going to have IO
-- call s i o = do
--   let fn = (stack s) !! (top s) -- TODO
--   let args = undefined -- next i in stack (I think???)
--   fn args -- assume fn always has access to state? Assume fn never has access to state?
--
-- -- Just replicate the C API?
-- call :: State -> Int -> Int -> IO (State)
-- call s argC resC = callK s argc resC, 0, Nothing)
--
-- callK :: State -> Int -> Int -> Int -> Maybe
-- -- void lua_callk (lua_State *L, int nargs, int nresults, lua_KContext ctx, lua_KFunction k)
--
-- absIndex :: State -> Int -> Int
-- absIndex s idx = if idx > 0 || isPseudoIndex idx then idx else (stackTop s) - undefined
-- -- return l.top - l.callInfo.function + index
--
--
-- -- data Closure = Closure {
-- --   upValues :: [Constant]
-- -- } deriving (Show)
--
-- -- apply :: VM -> OpCode -> VM
-- -- apply vm (OP_GETTABUP (Reg x) (UpIx y) (RK_Kst z)) = vm
-- -- apply vm (OP_LOADK (Reg regIndex) (Kst cIndex)) = vm { stack = newStack}
-- --   where theConstant = (constants vm) V.! cIndex
-- --         newStack = M.insert regIndex theConstant (stack vm)
-- -- apply vm (OP_CALL (Reg 0) (CountInt 1) (CountInt 0)) = vm
-- -- apply vm (OP_RETURN (Reg 0) (CountInt 0)) = vm
--

-- helloWorld = [
--   OP_GETTABUP (Reg 0) (UpIx 0) (RK_Kst (Kst 0)), -- Set Register 0 to Upvalues[0][Constant 0] (_ENV[print]) -- set 0 to print fn?
--   OP_LOADK (Reg 1) (Kst 1), -- set Register 1 to Constant 1 (set register 1 to "Hello World")
--   OP_CALL (Reg 0) (CountInt 1) (CountInt 0), -- Call function in Register 0 (print) with 1 arguments, 0 return, top set to last result plus 1
--   OP_RETURN (Reg 0) (CountInt 0) -- Return, 0 values
-- ]

set :: Int -> a -> Seq a -> Seq a
set i a s = case ((S.length s) <= i) of
  True -> S.insertAt i a s
  False -> S.update i a s

apply :: LState -> OpCode -> IO (LState)
apply s (OP_MOVE (Reg a) (Reg b)) = return $ s { stack = set a (fromJust $ a `S.lookup` stack s) (stack s)}
apply s (OP_LOADK (Reg a) (Kst b)) = do
  print $ "loadk " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show $ set a (constToValue $ (constants s) V.! b) (stack s))
  let newStack = set a (constToValue $ (constants s) V.! b) (stack s)
  let newTop = if (S.length newStack) > (S.length $ stack s) then (stackTop s )+ 1 else stackTop s
  return $ s { stack = newStack, stackTop = newTop }
apply s (OP_LOADKX (Reg a)) = undefined -- Not sure how to do this. Set a flag/stash the operation in state? (frame[i.a()] = constants[expectNext(ci, opExtraArg).ax()])
-- case opGetTableUp:
--   tmp := l.tableAt(closure.upValue(i.b()), k(i.c(), constants, frame))
--   frame = ci.frame
--   frame[i.a()] = tmp
apply s (OP_GETTABUP (Reg a) (UpIx b) (RK_Kst (Kst c))) = case ((constants s) V.! c) of
  (KString constName) -> do
    let upvalTable = (upValues s) !! b
    print "constName"
    print constName
    let upVal = upvalTable M.! constName
    return $ s { stack = set a (upVal) (stack s)}
  _ -> return s -- Probably fail, but do this later when I figure out better error reporting
apply s (OP_CALL (Reg a) (CountInt args) (CountInt rets)) = case fromJust $ a `S.lookup` stack s of
    HFunction f -> do
      print "executing function"
      -- (LState -> [Value] -> [Value])
      (newState, rets) <- f s
      -- every f should probably return (State, [rets]) or something. Seems a lot like time for a monad
      print "returned"
      print rets
      return newState
    _ -> do
      print "weird other thing, error"
      return s
apply s (OP_RETURN (Reg reg) (CountInt ct)) = return s --TODO
apply s (OP_CLOSURE (Reg reg) (ProtoIx ix)) = undefined


-- BUILT-IN FUNCTIONS
luaPrint :: LState -> IO (LState, [Value])
luaPrint s = do
  let n = lua_gettop s
  let theStack = stack s
  let valIndices = [1..n]
  print "luaPrint"
  print valIndices
  print n
  print theStack
  newState <- foldM (\nS i -> do
    let (newS, val) = pop nS -- maybe needs pop (index)
    print "printing val"
    print val
    return newS) s valIndices
  return (newState, [ LInt 0])

defaultUpvalues = M.fromList [("print", HFunction luaPrint), ("myPrint", HFunction luaPrint)]
main :: IO ()
main = do
  parse <- parseLuaBytecodeFile "luac.out"
  case (parse) of
    Left _ -> print "oh no"
    Right (Chunk _ c) -> do
      let theState = LState S.empty 1 0 (funcConstants c) (funcCode c) [defaultUpvalues] newCallInfo M.empty
      print (program theState)
      print "CHUNK"
      print c
      resultState <- foldM (apply) (theState) (program theState)
      print resultState
  -- print parse
-- -- Sample Program
-- -- [
-- --   OP_GETTABUP (Reg 0) (UpIx 0) (RK_Kst (Kst 0)), -- Set Register 0 to Upvalues[0][Constant 0] (_ENV[print]) -- set 0 to print fn?
-- --   OP_LOADK (Reg 1) (Kst 1), -- set Register 1 to Constant 1 (set register 1 to "Hello World")
-- --   OP_CALL (Reg 0) (CountInt 1) (CountInt 0), -- Call function in Register 0 (print) with 1 arguments, 0 return, top set to last result plus 1
-- --   OP_RETURN (Reg 0) (CountInt 0) -- Return, 0 values
-- -- ]
-- -- Constants
-- -- [
-- --   KString "print",
-- --   KString "Hello World"
-- -- ]

-- vmcase(OP_GETTABUP) {
--         const TValue *slot;
--         TValue *upval = cl->upvals[GETARG_B(i)]->v;
--         TValue *rc = KC(i);
--         TString *key = tsvalue(rc);  /* key must be a string */
--         if (luaV_fastget(L, upval, key, slot, luaH_getshortstr)) {
--           setobj2s(L, ra, slot);
--         }
--         else
--           Protect(luaV_finishget(L, upval, rc, ra, slot));
--         vmbreak;
--       }
