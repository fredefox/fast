{-
 - Interpreter for "Fast"
 - ======================
 -
 - This module implements an interpreter for the language Fast.
 -
 - This module follows the specfication outlined in the [hand-out].
 -
 - Written by Frederik Hanghøj Iversen
 - for the course Advanced Programming
 - at The University of Copenhagen 2014
 -
 -     me@fredefox.eu /^._
 -      ,___,--~~~~--' /'~
 -      `~--~\ )___,)/'
 -          (/\\_  (/\\_
 -
 -}
module FastInterpreter
    (runProg, Error (..)) where

import FastAST

import Text.Printf
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
    deriving (Show, Eq)

-- | Give the printed representation of a value.
printed :: Value -> String
printed (IntValue x) = show x
printed (StringValue s) = s
printed (ReferenceValue ref) = "#<object " ++ show ref ++ ">"
printed (TermValue (Term sym vs))
    = sym ++ "(" ++ intercalate ", " (map printed vs) ++ ")"

-- | A key-value store where the keys are of type @k@, and the values
-- are of type @v@.  Used for mapping object references to objects and
-- variable names to values.
type Store k v = Map.Map k v

-- | A mapping from object references to objects.
type GlobalStore = Store ObjectReference ObjectState

-- | A mapping from field names to field values.
type ObjectFields = Store Name Value

-- | A mapping from variable names to variable values.
type MethodVariables = Store Name Value

{-
 - The global state of the program execution.
 -
 - The global state must be the state of all the objects in the program.
 -}
data GlobalState
    = GlobalState {
        progState :: GlobalStore,
        output :: String,
        lastRef :: ObjectReference
    }

{-
 - The state of a single object.
 -}
data ObjectState
    = ObjectState {
        fields :: ObjectFields
   }

-- | The state of a method execution.
data MethodState
    = MethodState {
        varState :: MethodVariables
   }

{-
 - The basic monad in which execution of a Fast program takes place. Maintains
 - the global state, the running output, and whether or not an error has
 - occurred.
 -}
data FastM a = FastM {
        runFastM
            :: Prog
            -> GlobalState
            -> Either Error (Prog, GlobalState, a)
   }

instance Functor FastM where
    fmap = liftM

instance Applicative FastM where
    pure = return
    (<*>) = ap

instance Monad FastM where
    -- Just shove whatever `a` is inside this here monad.
    -- return :: a -> FastM a
    return a = FastM $ \p s -> Right (p, s, a)
    -- (>>=) :: FastM a -> (a -> FastM b) -> FastM b
    (FastM step) >>= f = FastM $ \p s -> either Left nextStep $ step p s where
        nextStep (p', s', a) = runFastM (f a) p' s'
    fail s = FastM $ const . const $ Left $ Error s

-- | Add the 'printed' representation of the value to the output.
printValue :: Value -> FastM ()
printValue v = FastM $ \p s -> Right (p, s' s, ()) where
    s' s = s {
        output = printed v ++ output s
    }

-- | Get the program being executed.
askProg :: FastM Prog
askProg = FastM $ \p s -> Right (p, s, p)

getGlobalState :: FastM GlobalState
getGlobalState = FastM $ \p s -> Right (p, s, s)

putGlobalState :: GlobalState -> FastM ()
putGlobalState s = FastM $ \p -> const $ Right (p, s, ())

modifyGlobalState :: (GlobalState -> GlobalState) -> FastM ()
modifyGlobalState f = FastM $ \p s -> Right (p, f s, ())

modifyGlobalStore :: (GlobalStore -> GlobalStore) -> FastM ()
modifyGlobalStore f = modifyGlobalState f' where
    -- This just applies `f` inside the progState of the globalState
    f' s = s { progState = f . progState $ s }

lookupObject :: ObjectReference -> FastM ObjectState
lookupObject ref = FastM $ look ref where
    look :: ObjectReference
        -> Prog -> GlobalState
        -> Either Error (Prog, GlobalState, ObjectState)
    look ref p s = case Map.lookup ref $ progState s of
        Nothing -> Left $ Error "Non-existing object"
        Just a -> Right (p, s, a)

setObject :: ObjectReference -> ObjectState -> FastM ()
setObject ref state = FastM $ set ref state where
    set :: ObjectReference
        -> ObjectState -> Prog
        -> GlobalState
        -> Either Error (Prog, GlobalState, ())
    set key value p s = Right $ (p, s', ()) where
        s' = s {
            progState = Map.insert key value $ progState s,
            lastRef = succ . lastRef $ s
        }

-- | Get a unique, fresh, never-before used object reference for use
-- to identify a new object.
allocUniqID :: FastM ObjectReference
allocUniqID = FastM $ \p s -> Right (p, s, succ . lastRef $ s)


-- | The monad in which methods (and constructors and receive actions)
-- execute.  Runs on top of 'FastM' - maintains the reference to self,
-- as well as the method variables.
--
-- Note that since FastMethodM runs on top of FastM, a FastMethodM
-- action has access to the global state (through liftFastM).
data FastMethodM a = FastMethodM {
        runFastMethodM :: ObjectReference -> MethodState -> FastM a
    }

instance Functor FastMethodM where
    fmap = liftM

instance Applicative FastMethodM where
    pure = return
    (<*>) = ap

instance Monad FastMethodM where
    return = liftFastM . return
    fail = liftFastM . fail
    -- (>>=) :: FastMethodM a -> (a -> FastMethodM b) -> FastMethodM b
    (FastMethodM step) >>= f = FastMethodM $ \ref s -> do
            a <- step ref s
            runFastMethodM (f a) ref s

-- | Perform a 'FastM' operation inside a 'FastMethodM'.
liftFastM :: FastM a -> FastMethodM a
-- op :: Prog -> GlobalState -> Either Error (GlobalState, a)
--
--     FastMethodM $ ObjectReference -> MethodState -> FastM ...
--
-- where I have chosen ... to be:
--
--     a
--
-- Type-checks, but probably not done.
liftFastM m = FastMethodM $ \ref s -> m

-- | Who are we? [Who! Who!](https://www.youtube.com/watch?v=PdLIerfXuZ4)
askSelf :: FastMethodM ObjectReference
-- Inside the monad there is a function that takes two arguments and returns
-- the first one of these (which is the object-reference) in monadic form.
askSelf = FastMethodM $ const . return

-- | Add the given name-value associations to the variable store.
bindVars :: [(Name, Value)] -> FastMethodM a -> FastMethodM a
bindVars assocs (FastMethodM f) =
    FastMethodM $ \ref os -> f ref (b os) where
        -- Performs the actual binding
        b :: MethodState -> MethodState
        b os = os { varState = m' } where
            curFields = varState os
            -- Insert all the elements from the list individually. The `Map`
            -- API surely might contain a method for actually doing this that
            -- is more effecient.
            m' :: ObjectFields
            m' = foldl (\m (k, v) -> Map.insert k v m) curFields assocs

getMethodState :: FastMethodM MethodState
getMethodState = FastMethodM $ const $ return

putMethodState :: MethodState -> FastMethodM ()
putMethodState s = liftFastM $ modifyGlobalState f where
    f = undefined -- TODO

getsMethodState :: (MethodState -> a) -> FastMethodM a
getsMethodState f = do
    s <- getMethodState
    return $ f s

modifyMethodState :: (MethodState -> MethodState) -> FastMethodM ()
modifyMethodState f = do
    s <- getMethodState
    putMethodState $ f s

getObjectState :: FastMethodM ObjectState
getObjectState = do
    s <- liftFastM $ getGlobalState
    ref <- askSelf
    case Map.lookup ref $ progState s of
        Nothing -> fail "No such ref"
        Just a -> return a

putObjectState :: ObjectState -> FastMethodM ()
putObjectState  s = do
    ref <- askSelf
    liftFastM $ modifyGlobalState $ mod ref where
        mod :: ObjectReference -> GlobalState -> GlobalState
        mod ref store = store { progState = Map.insert ref s $ progState store }

getsObjectState :: (ObjectState -> a) -> FastMethodM a
getsObjectState f = do
    s <- getObjectState
    return $ f s

modifyObjectState :: (ObjectState -> ObjectState) -> FastMethodM ()
modifyObjectState f = do
    s <- getObjectState
    putObjectState $ f s


-- | Find the declaration of the class with the given name, or cause
-- an error if that name is not a class.
findClassDecl :: Name -> FastM ClassDecl
findClassDecl n = do
    prog <- askProg
    let p :: ClassDecl -> Bool
        p c = className c == n in
        case find p prog of
            Nothing -> fail $ printf "No class declaration with name `%s`" n
            Just a -> return a

-- | Instantiate the class with the given name, passing the given
-- values to the constructor.
createObject :: Name -> [Value] -> FastM ObjectReference
createObject n vals = do
    decl <- findClassDecl n
    ref <- allocUniqID
    setObject ref initial
    return ref where
        initial :: ObjectState
        initial = ObjectState {
            fields = Map.empty
        }

sendMessageTo :: Value -> Value -> FastM Value
sendMessageTo = undefined

-- | Evaluate a method body - the passed arguments are the object in
-- which to run, the initial variable bindings (probably the
-- parameters of the method, constructor or receive action), and the
-- body.  Returns a value and the new state of the object.
evalMethodBody
    :: ObjectReference
    -> [(Name, Value)]
    -> Exprs
    -> FastM (Value, ObjectState)
evalMethodBody ref params body = do
    v <- runFastMethodM (evalExprs body) ref $ MethodState $ Map.fromList params
    s <- lookupObject ref
    modifyGlobalState $ (\gs -> gs { output = output gs ++ printed v })
    return (v, s)

evalExprs :: [Expr] -> FastMethodM Value
evalExprs [] = return $ TermValue $ Term "nil" []
evalExprs [e] = evalExpr e
evalExprs (e:es) = evalExpr e >> evalExprs es

evalExpr :: Expr -> FastMethodM Value
evalExpr e = case e of
    (IntConst i) -> return $ IntValue i
    (StringConst s) -> return $ StringValue s
    (TermLiteral name es) -> return $ TermValue $ Term name (esToVs es) where
        -- TODO: `es` are expressions - they should be mapped to values.
        -- `evalExprs`s left-hand side match but it only returns one value
        -- where the constructor `Term` expects an array of values.
        --
        -- Maybe we should `evalMethodBody` on ourselves (`askSelf`). Yet this
        -- method requires the names of the aruments.
        esToVs :: [Expr] -> [Value]
        esToVs = undefined
    (Self) -> fmap ReferenceValue askSelf
    (Plus e0 e1) -> do
        -- If `e0` and `e1` do not evaluate to integer values. Well... then we
        -- are in trouble. This should only be caused by a *semantically*
        -- incorrect program.
        (IntValue v0) <- evalExpr e0
        (IntValue v1) <- evalExpr e1
        return $ IntValue $ v0 + v1
    (Minus e0 e1) -> do
        (IntValue v0) <- evalExpr e0
        (IntValue v1) <- evalExpr e1
        return $ IntValue $ v0 - v1
    (Times e0 e1) -> do
        (IntValue v0) <- evalExpr e0
        (IntValue v1) <- evalExpr e1
        return $ IntValue $ v0 * v1
    (DividedBy e0 e1) -> do
        (IntValue v0) <- evalExpr e0
        (IntValue v1) <- evalExpr e1
        return $ IntValue $ v0 `div` v1
    (Return e) -> evalExpr e
    -- Use `modifyMethodState`
    (SetField fld e) -> do
        v <- evalExpr e
        modifyObjectState $ mod v
        return v where
            mod :: Value -> ObjectState -> ObjectState
            mod v (ObjectState flds)
                = ObjectState $ Map.insert fld v flds
    (SetVar fld e) -> do
        v <- evalExpr e
        modifyMethodState $ mod v
        return v where
            mod v (MethodState vars)
                = MethodState $ Map.insert fld v vars
    (ReadVar var) -> do
        s <- getMethodState
        maybe (fail "Non-existing variable") return
            $ Map.lookup var $ varState s
    (ReadField fld) -> do
        s <- getObjectState
        maybe (fail "Non-existing field") return
            $ Map.lookup fld $ fields s
    (Match e cs) -> do
        v <- evalExpr e
        match v cs where
            match :: Value -> Cases -> FastMethodM Value
            match _ [] = fail "No matching case-expression"
            match v ((p, e):ps) = if m v p
                then evalExprs e
                else match v ps where
                    m (IntValue x) (ConstInt y) = x == y
                    m (StringValue x) (ConstString y) = x == y
                    -- TODO: Don't know how to match the rest.
                    m _ _ = False
    (SendMessage rcvr {-< Receiver -} msg) -> undefined {-< The message -}
    -- `o`: Object, `m`: Methods, `ps`: parameters
    (CallMethod o m ps) -> undefined
    {-
     - This seems like rubbish:
     -
        do
        case evalExpr of
            ObjectReference ref ->
                decl <- findClassDecl m
                liftFastM $ evalMethodBody ref (bla decl) body where
                    bla decl = classMethods decl
            x -> fail $ printf "Cannot call method on `s`" [show x]
     -}
    (New n es) -> do
        ref <- liftFastM $ createObject n $ evls es
        undefined where
            evls :: [Expr] -> [Value]
            evls = undefined

runProg :: Prog -> Either Error String
runProg prog = let
    initM = do
        ref <- createObject "Main" []
        decl <- findClassDecl "Main"
        case classConstructor decl of
            Nothing -> fail "No method in class constructor"
            Just a -> evalMethodBody ref [] $ methodBody a
    init = GlobalState {
        progState = Map.empty,
        output = "",
        lastRef = 0
    } in
    case runFastM initM prog init of
        Left e -> Left e
        Right (_, a, _) -> Right $ output a
