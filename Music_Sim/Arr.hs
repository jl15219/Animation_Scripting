module Arr where


--
type SLS a = ([a],[a])

type Arr a = SLS (SLS a)

fmapsls :: (a -> b) -> SLS a -> SLS b
fmapsls f (as,bs) = (fmap f as, fmap f bs)

fmapArr :: (a -> b) -> Arr a -> Arr b
fmapArr f (ass) = fmapsls (fmapsls f) ass

slsDeg :: SLS a -> b -> (a -> b -> c) -> (b -> b) -> SLS c
slsDeg ([],gs) b f h = ([], gs') where
  ([], gs') = slsDegr ([],gs) (h b) f h
slsDeg (a:as,gs) b f h = ((f a b) : as', gs') where
  (as', gs') = slsDegr (as,gs) (h b) f h

slsDegr :: SLS a -> b -> (a -> b -> c) -> (b -> b) -> SLS c
slsDegr ([],[]) _ _ _ = ([],[])
slsDegr ([],g:gs) b f h = ([], (f g b) : gs') where
  ([], gs') = slsDegr ([],gs) (h b) f h
slsDegr (a:as,[]) b f h = ((f a b) : as', []) where
  (as', []) = slsDegr (as,[]) (h b) f h
slsDegr (a:as,g:gs) b f h = ((f a b) : as', (f g b) : gs') where
  (as', gs') = slsDegr (as,gs) (h b) f h

incByNext :: (a -> a -> a) -> [a] -> [a]
incByNext _ []       = []
incByNext _ (a:[])   = [a]
incByNext f (a:b:as) = (f a b) : (incByNext f (b:as))

arrDeg :: Arr a -> b -> (a -> b -> c) -> (b -> b) -> Arr c
arrDeg ([],gs) b f h   = ([],gs') where
  ([],gs')  = arrDegr ([],gs) (h b) f h
arrDeg (a:as,gs) b f h = (k:as',gs') where
  k = slsDeg a b f h
  (as',gs') = arrDegr (as,gs) (h b) f h

arrDegr :: Arr a -> b -> (a -> b -> c) -> (b -> b) -> Arr c
arrDegr ([],[]) _ _ _     = ([],[])
arrDegr ([],g:gs) b f h   = ([], (k g): gs') where
  ([],gs') = arrDegr ([],gs) (h b) f h
  k = \z -> slsDeg z b f h
arrDegr (a:as,[]) b f h   = ((k a) : as', []) where
  (as',[]) = arrDegr (as,[]) (h b) f h
  k = \z -> slsDeg z b f h
arrDegr (a:as,g:gs) b f h = ((k a) : as', (k g) : gs') where
  (as', gs') = arrDegr (as,gs) (h b) f h
  k = \z -> slsDeg z b f h

arrTranDeg :: Arr a -> Arr b -> (a -> b -> a) -> (b -> b) -> Arr a
arrTranDeg ass (([],ds):dss,[])    f h = ass
arrTranDeg ass ([],dss)            f h = arrTranDeg (sTghArr ass) (sTghArr ([],dss)) f h
arrTranDeg ass (([],ms):bss,dss)   f h = arrTranDeg (sTghArr ass) (sTghArr (([],ms):bss,dss)) f h
arrTranDeg ass ((b:bs,ms):bss,dss) f h = arrTranDeg (sTghArr (arrDeg ass b f h)) (sTghArr ((b:bs,ms):bss,dss)) f h
-- Need to do a shift2Front for future rows
arrTranDegFast :: Arr a -> Arr [b] -> (a -> [b] -> a) -> ([b] -> [b]) -> Arr a
arrTranDegFast ass (([],bs):bss,[])     f h  = ass
arrTranDegFast ass ([],dss)             f h  = arrTranDegFast (sTghArr ass) (sTghArr ([],dss)) f h
arrTranDegFast ass (([],ms):bss,dss)    f h  = arrTranDegFast (sTghArr ass) (sTghArr (([],ms):bss,dss)) f h
arrTranDegFast ass (([]:bs,ms):bss,dss) f h  = arrTranDegFast (sTghArr ass) (sTghArr (([]:bs,ms):bss,dss)) f h
arrTranDegFast ass ((b:bs,ms):bss,dss)  f h  = arrTranDegFast (sTghArr (arrDeg ass b f h)) (sTghArr ((b:bs,ms):bss,dss)) f h


arrTranDegTest :: Integer -> Arr a -> Arr b -> (a -> b -> a) -> (b -> b) -> (Arr a, Arr b)
arrTranDegTest 0 ass bss                 f h = (ass,bss)
arrTranDegTest n ass (([],ds):dss,[])    f h = (ass,((ds,[]):dss,[]))
arrTranDegTest n ass ([],dss)            f h = arrTranDegTest (n-1) (sTghArr ass) (sTghArr ([],dss)) f h
arrTranDegTest n ass (([],ms):bss,dss)   f h = arrTranDegTest (n-1) (sTghArr ass) (sTghArr (([],ms):bss,dss)) f h
arrTranDegTest n ass ((b:bs,ms):bss,dss) f h = arrTranDegTest (n-1) (sTghArr (arrDeg ass b f h)) (sTghArr ((b:bs,ms):bss,dss)) f h

zipWithArr :: (a -> b -> c) -> Arr a -> Arr b -> Arr c
zipWithArr f (pss,gss) (dss,bss) = (combSLSs f pss dss,combSLSs f gss bss)

combSLSs :: (a -> b -> c) -> [SLS a] -> [SLS b] -> [SLS c]
combSLSs f [] _ = []
combSLSs f _ [] = []
combSLSs f (a:as) (b:bs) = (combSLS f a b) : (combSLSs f as bs)

combSLS :: (a -> b -> c) -> SLS a -> SLS b -> SLS c
combSLS f (gs,ps) (ds,bs) = (zipWith f gs ds, zipWith f ps bs)

shift2Front :: SLS a -> SLS a
shift2Front ([],as) = ([],as)
shift2Front (a:as,bs) = shift2Front (as,a:bs)

sTghArr :: Arr a -> Arr a
sTghArr (as,[])   = (as,[])
sTghArr ([],b:bs) = ([b],bs)
sTghArr (((gs,[]):as),b:bs) = (fmap shift2Front (b:(gs,[]):as), fmap shift2Front (bs))
sTghArr (as,bs) = (fmap stepThroughSLS as, fmap stepThroughSLS bs)

-- ([],[([],[1,2,3,4]),([],[5,6,7,8])])
--      0
--      1 2 3 4
--      5 6 7 8
-- ( [([] , [1,2,3,4])], [([] , [5,6,7,8])] )
--   0  1 2 3 4
--      5 6 7 8
-- ( [([1] , [2,3,4])], [([5] , [6,7,8])] )
--      0 2 3 4
--      5 6 7 8
-- ( [([2,1] , [3,4])], [([6,5] , [7,8])])
--      1 0 3 4
--      5 6 7 8
-- ( [([3,2,1] , [4])], [([7,6,8] , [8])])
--      1 2 0 4
--      5 6 7 8
-- ( [([4,3,2,1] , [])], [([8,7,6,5] , [])])
--      1 2 3 0
--      5 6 7 8
-- ( [ ([],[5,6,7,8]) , ([] , [1,2,3,4])], [])
--      1 2 3 4
--   0  5 6 7 8
-- ( [ ([5],[6,7,8]) , ([1] , [2,3,4])], [])
--      1 2 3 4
--      0 6 7 8

stepThroughSLS :: SLS a -> SLS a
stepThroughSLS (as,[])   = (as,[])
stepThroughSLS (as,b:bs) = (b:as,bs)

reset :: a -> Arr b -> Arr a
reset a = pushThrough (fmap (pushThrough (fmap (const a))))

pushThrough :: ([a] -> [b]) -> SLS a -> SLS b
pushThrough f (as,bs) = (f as, f bs)

slsToL :: SLS a -> [a]
slsToL (as,bs) = reverse as ++ bs

arrToLofL :: Arr a -> [[a]]
arrToLofL (ass,bss) = (fmap slsToL (reverse ass)) ++ (fmap slsToL bss)

-- class Pretty a where
--   pretty :: a -> String
--
-- instance Pretty a => Pretty (SLS a) where
--   pretty (as,bs) = show ((reverse (fmap show as)) ++ (fmap show bs))
--
-- instance Pretty a => Pretty (Arr a) where
--   pretty (as,bs) = (fmap ((++ "\n") . pretty) reverse as) ++ (fmap ((++ "\n") . pretty) bs)


searchArr :: (a -> Bool) -> Arr a -> Arr a
searchArr f (ass,[])    = (ass,[])
searchArr f (((a:as,ms):ass),bss) | f a       = (((a:as,ms):ass),bss)
                                  | otherwise = searchArr f (sTghArr (((a:as,ms):ass),bss))
searchArr f kss         = searchArr f (sTghArr kss)
