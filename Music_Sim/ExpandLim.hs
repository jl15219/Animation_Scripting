
import Arr

data EO = Exp | Lim | NoTerr
  deriving (Eq, Show)

expandOut :: Arr EO -> Arr EO
expandOut (ass,[]) = (ass,[])
expandOut (ass,bss) = expandOut (expOut (searchArr ((==) Exp) ((ass),(bss))))

expOut :: Arr EO -> Arr EO
expOut (((Exp:gs,ps):gss),(dss)) = ((Lim : (expOutList gs), expOutList ps) : expOut2 (gss), expOut2 dss)
expOut a = a

expOutList :: [EO] -> [EO]
expOutList (Exp:as) = (Exp:as)
expOutList (Lim:as) = (Lim:as)
expOutList (NoTerr:as) = Lim : (expOutList as)

expOut2 :: [SLS EO] -> [SLS EO]
expOut2 [] = []
expOut2 ((Lim:gs,ps):gss) = ((Lim:gs,ps):gss)
expOut2 ((gs,ps):gss)     = ((expOutList gs, expOutList ps): (expOut2 gss))


data EO a = Exp a | Lim a | NoTerr
  deriving (Eq, Show)

expandOutA :: Arr (EO a) -> Arr (EO a)
expandOutA (ass,[]) = (ass,[])
expandOutA (ass,bss) = expandOutA (expOutA (searchArr ((==) (Exp _)) ((ass),(bss))))

expOutA :: Arr (EO a) -> Arr (EO a)
expOutA ((((Exp a):gs,ps):gss),(dss)) = (((Lim a) : (expOutListA gs a), expOutListA ps a) : expOut2A (gss) a, expOut2A dss a)
expOutA a = a

expOutListA :: [(EO a)] -> a -> [(EO a)]
expOutListA ((Exp a):as) b = ((Exp a):as)
expOutListA ((Lim a):as) b = ((Lim a):as)
expOutListA (NoTerr:as)  b = (Lim b) : (expOutListA as)

expOut2A :: [SLS (EO a)] -> a -> [SLS (EO a)]
expOut2A [] _ = []
expOut2A (((Lim a):gs,ps):gss) _ = (((Lim a):gs,ps):gss)
expOut2A ((gs,ps):gss) b         = ((expOutListA gs b, expOutListA ps b): (expOut2A gss b))
