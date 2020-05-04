(declare-sort E)
(declare-datatypes () ((I (in1) (in2) (in3) (in4); (in5)
  )) )
(declare-sort V)
(declare-sort S)

(declare-datatypes () ((EventType (R) (W) (U))))
(declare-datatypes () ((MethodType (Enq)(Deq) )))
(declare-datatypes () ((FieldType (Default)(Val)(Next) )))
(declare-datatypes () ((StmtType (E1t) (E2t) (E3t) (E4t) (E5t) (E6t) (E7t) (D1t) (D2t) (D3t) (D4t) (D5t) (D6t) (D7t) (Bot) )))


(declare-fun newloc (I) V)
(declare-fun etype (E) EventType)
(declare-fun stype (E) StmtType)
(declare-fun loc (E) V)
(declare-fun field (E) FieldType)
(declare-fun rval (E) V)
(declare-fun wval (E) V)
(declare-fun vis (E E) Bool)
(declare-fun ar (E E) Bool)
(declare-fun soE (E E) Bool)

(declare-fun rf (E E) Bool)

(declare-fun soI (I I) Bool)
(declare-fun sess (I) S)
(declare-fun argval (I) V)
(declare-fun retval (I) V)
(declare-fun itype (I) MethodType)
(declare-fun completed (I) Bool)

(declare-fun D1e (I) E)
(declare-fun D2e (I) E)
(declare-fun D3e (I) E)
(declare-fun D4e (I) E)
(declare-fun D5e (I) E)
(declare-fun D6e (I) E)
(declare-fun D7e (I) E)

(declare-fun E1e (I) E)
(declare-fun E2e (I) E)
(declare-fun E3e (I) E)
(declare-fun E4e (I) E)
(declare-fun E5e (I) E)
(declare-fun E6e (I) E)
(declare-fun E7e (I) E)

(define-fun isM ((e E) ) Bool
(or (= (etype e) W) (= (etype e) U))
)
(define-fun isR ((e E)) Bool
(or (= (etype e) U)(= (etype e) R))
)
(define-fun isBot ((e E)) Bool
(= (stype e) Bot)
  )
(define-fun sameloc ((e1 E) (e2 E)) Bool
(and (= (field e1) (field e2)) (= (loc e1) (loc e2)))
)
(define-fun sameses ((i1 I) (i2 I)) Bool
(= (sess i1) (sess i2))
)

(declare-fun initval (V FieldType) V)
(declare-fun rfinit (E) Bool)


;Local Variables
(declare-fun enqLast (I) V)
(declare-fun enqNext (I) V)
(declare-fun deqFirst (I) V)
(declare-fun deqLast (I) V)
(declare-fun deqNext (I) V)
(declare-fun deqRetval (I) V)


;Locations
(declare-const tail V)
(declare-const head V)
(declare-const sentinelNode V)

;Values
(declare-const zero V)
;(declare-const one V)
(declare-const EMPTY V)
(declare-const NULL V)


;Last Writer Wins
;(assert (forall ((e1 E)) (exists ((e2 E)) (=> (isR e1) (or (and (rfinit e1) (= (rval e1) (initval (loc e1) (field e1)))) (and (isM e2) (sameloc e1 e2) (vis e2 e1) (= (wval e2) (rval e1)) (rf e2 e1) )) ) ) ))
(assert (forall ((e E)) (=> (isR e) (or (rf (D1e in1) e) (rf (D2e in1) e) (rf (D3e in1) e) (rf (D4e in1) e) (rf (D5e in1) e) (rf (D6e in1) e) (rf (D7e in1) e) (rf (D1e in2) e) (rf (D2e in2) e) (rf (D3e in2) e) (rf (D4e in2) e) (rf (D5e in2) e) (rf (D6e in2) e) (rf (D7e in2) e)
(rf (D1e in3) e) (rf (D2e in3) e) (rf (D3e in3) e) (rf (D4e in3) e) (rf (D5e in3) e) (rf (D6e in3) e) (rf (D7e in3) e)
(rf (D1e in4) e) (rf (D2e in4) e) (rf (D3e in4) e) (rf (D4e in4) e) (rf (D5e in4) e) (rf (D6e in4) e) (rf (D7e in4) e)
;(rf (D1e in5) e) (rf (D2e in5) e) (rf (D3e in5) e) (rf (D4e in5) e) (rf (D5e in5) e) (rf (D6e in5) e) (rf (D7e in5) e)
  (rf (E1e in1) e) (rf (E2e in1) e) (rf (E3e in1) e) (rf (E4e in1) e) (rf (E5e in1) e) (rf (E6e in1) e) (rf (E7e in1) e)
    (rf (E1e in2) e) (rf (E2e in2) e) (rf (E3e in2) e) (rf (E4e in2) e) (rf (E5e in2) e) (rf (E6e in2) e) (rf (E7e in2) e)
     (rf (E1e in3) e) (rf (E2e in3) e) (rf (E3e in3) e) (rf (E4e in3) e) (rf (E5e in3) e) (rf (E6e in3) e) (rf (E7e in3) e)
      (rf (E1e in4) e) (rf (E2e in4) e) (rf (E3e in4) e) (rf (E4e in4) e) (rf (E5e in4) e) (rf (E6e in4) e) (rf (E7e in4) e)
  ; (rf (E1e in5) e) (rf (E2e in5) e) (rf (E3e in5) e) (rf (E4e in5) e) (rf (E5e in5) e) (rf (E6e in5) e) (rf (E7e in5) e)
      (rfinit e)) ) ))
(assert (forall ((e E)) (=> (rfinit e) (= (rval e) (initval (loc e) (field e)))) ))
(assert (forall ((e1 E) (e2 E)) (=> (and (vis e1 e2) (sameloc e1 e2) (isM e1) (not (= (stype e1) Bot)) ) (not (rfinit e2)) ) ))
(assert (forall ((e1 E) (e2 E)) (=> (and (isM e1) (isM e2) (sameloc e1 e2) (not (= e1 e2)) ) (or (ar e1 e2) (ar e2 e1))) ))
(assert (forall ((e1 E) (e2 E)) (=>  (ar e1 e2)  (not (ar e2 e1))) ))
(assert (forall ((e1 E) (e2 E)) (=> (ar e1 e2) (and (sameloc e1 e2) (isM e1) (isM e2) ) ) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (ar e1 e2) (ar e2 e3))  (ar e1 e3)) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (isR e1) (rf e2 e1) (vis e3 e1) (not (= (stype e3) Bot)) (not (= e3 e2)) (isM e3) (sameloc e2 e3)) (ar e3 e2) ) ))
(assert (forall ((e1 E) (e2 E)) (=>  (rf e1 e2) (and (vis e1 e2) (not (= (stype e1) Bot)) (= (wval e1) (rval e2)) (sameloc e1 e2) (isM e1) (isR e2) ) )) )
(assert (forall ((e1 E) (e2 E)) (=> (vis e1 e2) (not (vis e2 e1)))))
(assert (forall ((e1 E) (e2 E)) (=> (vis e1 e2) (not (soE e2 e1))) ))

;CAS Semantics
(assert (forall ((e E) (e1 E) (e2 E)) (=> (and (= (etype e1) U) (= (etype e2) U) (sameloc e1 e2) (rf e e1) (rf e e2) ) (= e1 e2) ) ))
(assert (forall ((e1 E) (e2 E)) (=> (and (= (etype e1) U) (= (etype e2) U) (sameloc e1 e2) (rfinit e1) (rfinit e2) ) (= e1 e2) ) ))

;No cycle on vis+soE
(declare-fun tot (E E) Bool)
(assert (forall ((e1 E) (e2 E)) (=> (vis e1 e2)  (tot e1 e2)) ))
(assert (forall ((e1 E) (e2 E)) (=> (soE e1 e2)  (tot e1 e2)) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (tot e1 e2) (tot e2 e3))  (tot e1 e3)) ))
(assert (forall ((e1 E) (e2 E)) (=>  (tot e1 e2)  (not (tot e2 e1))) ))
(assert (forall ((e1 E) (e2 E)) (not (and (tot e1 e2) (ar e2 e1))) ))


;Session Order on Events
(assert (forall ((e1 E) (e2 E)) (=>  (soE e1 e2)  (not (soE e2 e1))) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (soE e1 e2) (soE e2 e3))  (soE e1 e3)) ))

;Session Order on Invocations
(assert (forall ((i1 I) (i2 I)) (=> (and (sameses i1 i2) (not (= i1 i2))) (or (soI i1 i2) (soI i2 i1)) ) ))
(assert (forall ((i1 I) (i2 I)) (=>  (soI i1 i2)  (not (soI i2 i1))) ))
(assert (forall ((i1 I) (i2 I)) (=>  (soI i1 i2)  (sameses i1 i2)) ))
(assert (forall ((e1 I) (e2 I) (e3 I)) (=> (and (soI e1 e2) (soI e2 e3))  (soI e1 e3)) ))

(define-fun lastevent ((i I)) E
(ite (= (itype i) Enq) (ite (not (isBot (E7e i))) (E7e i) (ite (not (isBot (E6e i))) (E6e i) (ite (not (isBot (E5e i))) (E5e i) (E4e i)  ) ) )
 (ite (not (isBot (D5e i))) (D5e i) (ite (not (isBot (D7e i))) (D7e i) (D4e i) )  )   )
)
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i2) Enq) (soI i1 i2)) (soE (lastevent i1) (E1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i2) Deq) (soI i1 i2)) (soE (lastevent i1) (D1e i2)))))

(assert (not (= tail sentinelNode)))
(assert (not (= head sentinelNode)))
(assert (not (= tail head)))
(assert (not (= zero EMPTY)))

(assert (not (= NULL sentinelNode)))



(assert (= (initval tail Default) sentinelNode ))
(assert (= (initval head Default) sentinelNode ))
(assert (forall ((l V)) (= (initval l Next) NULL) ))
(assert (forall ((l V)) (= (initval l Val) zero) ))


(assert (forall((i I)) (not (= (argval i) EMPTY))))
(assert (forall((i I)) (not (= (argval i) zero))))
(assert (forall((i I)) (not (= (newloc i) NULL))))
(assert (forall((i I)) (not (= (newloc i) sentinelNode))))
(assert (forall ((i1 I) (i2 I)) (=> (= (newloc i1) (newloc i2)) (= i1 i2) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D1e i)) (deqFirst i)) (= (stype (D1e i)) D1t) (= (loc (D1e i)) head) (= (field (D1e i)) Default) (= (etype (D1e i)) R)) ) ))

(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D1e i) (D2e i)) ) ) )
(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D2e i)) (deqLast i)) (= (stype (D2e i)) D2t) (= (loc (D2e i)) tail) (= (field (D2e i)) Default) (= (etype (D2e i)) R)) ) ))

(assert (forall ((i I)) (=> (= (itype i) Deq)  (soE (D2e i) (D3e i))) ) )
(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D3e i)) (deqNext i)) (= (stype (D3e i)) D3t) (= (loc (D3e i)) (deqFirst i)) (= (field (D3e i)) Next) (= (etype (D3e i)) R)) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D3e i) (D4e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (stype (D4e i)) D4t) (= (loc (D4e i)) head) (= (field (D4e i)) Default) (= (etype (D4e i)) R)) ) ))


(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (= (deqFirst i) (deqLast i)) (= (deqNext i) NULL) ) (and (completed i) (= (retval i) EMPTY) (isBot (D5e i)) (isBot (D6e i)) (isBot (D7e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (= (deqFirst i) (deqLast i)) (not (= (deqNext i) NULL)) ) (and (not (completed i)) (isR (D5e i)) (= (stype (D5e i)) D5t) (isBot (D6e i)) (isBot (D7e i)) (= (loc (D5e i)) tail) (= (field (D5e i)) Default) (soE (D4e i) (D5e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (= (deqFirst i) (deqLast i)) (not (= (deqNext i) NULL)) (= (rval (D5e i)) (deqLast i)) ) (and (= (etype (D5e i)) U) (= (wval (D5e i)) (deqNext i) ) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (= (deqFirst i) (deqLast i)) (not (= (deqNext i) NULL)) (not (= (rval (D5e i)) (deqLast i))) ) (= (etype (D5e i)) R)   ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (not (= (deqFirst i) (deqLast i))) (= (deqNext i) NULL) ) (and  (isBot (D5e i)) (isBot (D6e i))(isBot (D7e i)) (= (retval i) EMPTY) (completed i) ) )))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (not (= (deqFirst i) (deqLast i))) (not (= (deqNext i) NULL)) ) (and  (isBot (D5e i)) (= (etype (D6e i)) R) (= (loc (D6e i)) (deqNext i)) (= (field (D6e i)) Val) (= (stype (D6e i)) D6t) (= (rval (D6e i)) (deqRetval i) ) (soE (D4e i) (D6e i)) ) )))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (not (= (deqFirst i) (deqLast i))) (not (= (deqNext i) NULL)) ) (and  (isR (D7e i)) (= (loc (D7e i)) head) (= (field (D7e i)) Default) (= (stype (D7e i)) D7t)  (soE (D6e i) (D7e i)) ) )))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (not (= (deqFirst i) (deqLast i)))  (= (rval (D7e i)) (deqFirst i) ) (not (= (deqNext i) NULL)) ) (and  (= (etype (D7e i)) U) (= (wval (D7e i)) (deqNext i)) (completed i) (= (retval i) (deqRetval i)) ) )))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (rval (D4e i)) (deqFirst i)) (not (= (deqFirst i) (deqLast i))) (not (= (rval (D7e i)) (deqFirst i) )) (not (= (deqNext i) NULL)) ) (and  (= (etype (D7e i)) R) (not (completed i))  ) )))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (rval (D4e i)) (deqFirst i))) ) (and (isBot (D5e i)) (isBot (D6e i)) (isBot (D7e i)) (not (completed i)) ) )))


(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D1e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D2e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D3e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D4e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D5e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D6e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D7e i)) Bot ) )))

(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E1e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E2e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E3e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E4e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E5e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E6e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E7e i)) Bot ) )))

(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E1e i)) W)  (= (loc (E1e i)) (newloc i)) (= (stype (E1e i)) E1t) (= (field (E1e i)) Val) (= (wval (E1e i)) (argval i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E1e i) (E2e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E2e i)) R)  (= (loc (E2e i)) tail) (= (stype (E2e i)) E2t) (= (field (E2e i)) Default) (= (rval (E2e i)) (enqLast i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E2e i) (E3e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E3e i)) R)  (= (loc (E3e i)) (enqLast i)) (= (stype (E3e i)) E3t) (= (field (E3e i)) Next) (= (rval (E3e i)) (enqNext i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E3e i) (E4e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E4e i)) R)  (= (loc (E4e i)) tail) (= (stype (E4e i)) E4t) (= (field (E4e i)) Default)  ) ) ))


;(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E4e i) (E5e i)) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (= (enqNext i) NULL) ) (and (isR (E5e i)) (= (loc (E5e i)) (enqLast i) ) (= (field (E5e i)) Next) (= (stype (E5e i)) E5t) (isBot (E7e i)) (soE (E4e i) (E5e i)) ) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (= (enqNext i) NULL) (= (rval (E5e i)) (enqNext i) ) ) (and  (= (etype (E5e i)) U) (= (wval (E5e i)) (newloc i) ) (isR (E6e i)) (= (stype (E6e i)) E6t) (= (loc (E6e i)) tail ) (= (field (E6e i)) Default ) (soE (E5e i) (E6e i)) (completed i) ) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (= (enqNext i) NULL) (not (= (rval (E5e i)) (enqNext i) )) ) (and  (= (etype (E5e i)) R) (isBot (E6e i) ) (not (completed i)) ) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (= (enqNext i) NULL) (= (rval (E5e i)) (enqNext i) ) (= (rval (E6e i)) (enqLast i)) ) (and  (= (etype (E6e i)) U) (= (wval (E6e i)) (newloc i) )   ) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (= (enqNext i) NULL) (= (rval (E5e i)) (enqNext i) ) (not (= (rval (E6e i)) (enqLast i))) )  (= (etype (E6e i)) R)     ) ))


(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (not (= (enqNext i) NULL)) ) (and (isBot (E5e i)) (isBot (E6e i)) (isR (E7e i)) (= (loc (E7e i)) tail) (= (field (E7e i)) Default) (= (stype (E7e i)) E7t ) (soE (E4e i) (E7e i)) (not (completed i)) ) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (not (= (enqNext i) NULL))  (= (rval (E7e i)) (enqLast i) ) ) (and (= (etype (E7e i)) U) (= (wval (E7e i)) (enqNext i) ) ) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (= (rval (E4e i)) (enqLast i)) (not (= (enqNext i) NULL))  (not (= (rval (E7e i)) (enqLast i) )) ) (= (etype (E7e i)) R)   ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Enq) (not (= (rval (E4e i)) (enqLast i))) ) (and (isBot (E5e i)) (isBot (E6e i)) (isBot (E7e i)) (not (completed i)) ) ) ))


(assert (forall ((i1 I) (i2 I)) (=> (= (D1e i1) (D1e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D2e i1) (D2e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D3e i1) (D3e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D4e i1) (D4e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D5e i1) (D5e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D6e i1) (D6e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D7e i1) (D7e i2)) (= i1 i2) ) ))

(assert (forall ((i1 I) (i2 I)) (=> (= (E1e i1) (E1e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E2e i1) (E2e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E3e i1) (E3e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E4e i1) (E4e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E5e i1) (E5e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E6e i1) (E6e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E7e i1) (E7e i2)) (= i1 i2) ) ))

;Queue Specification
(declare-fun matchm (I I) Bool)
(assert (forall ((i1 I) (i2 I)) (= (matchm i1 i2) (and (= (itype i1) Enq) (= (itype i2) Deq) (completed i2) (= (argval i1) (retval i2)))) ))
(assert (forall ((i1 I) (i2 I)) (=> (not (= i1 i2))(not (= (argval i1) (argval i2)))) ))

;AddRem
; (assert (= (itype in1) Deq))
; (assert (forall ((i I)) (and (not (= (retval in1) EMPTY)) (completed in1) (not (matchm i in1)) ) ))


;Injective
;(assert (matchm in1 in2))
;(assert (matchm in1 in3))

;Empty
(declare-fun bef (I I) Bool)

 (assert (forall ((i1 I) (i2 I)) (=>  (bef i1 i2)  (not (bef i2 i1))) ))
 (assert (forall ((e1 I) (e2 I) (e3 I)) (=> (and (bef e1 e2) (bef e2 e3))  (bef e1 e3)) ))

 (assert (forall ((i1 I) (i2 I)) (=>  (matchm i1 i2)  (bef i1 i2))) )
 (assert (forall ((i1 I) (i2 I)) (=> (soI i1 i2)  (bef i1 i2))) )

;(assert (forall ((i1 I) (i2 I)) (exists ((i3 I)) (=> (bef i1 i2) (or (soI i1 i2) (matchm i1 i2) (and (bef i1 i3) (bef i3 i2))))   ) ))
(assert (=> (bef in1 in2) (or (soI in1 in2) (matchm in1 in2)(and (bef in1 in3) (bef in3 in2))(and (bef in1 in4) (bef in4 in2)))))
(assert (=> (bef in1 in3) (or (soI in1 in3) (matchm in1 in3)(and (bef in1 in2) (bef in2 in3))(and (bef in1 in4) (bef in4 in3)))))
(assert (=> (bef in1 in4) (or (soI in1 in4) (matchm in1 in4)(and (bef in1 in2) (bef in2 in4))(and (bef in1 in3) (bef in3 in4)))))
(assert (=> (bef in2 in1) (or (soI in2 in1) (matchm in2 in1)(and (bef in2 in3) (bef in3 in1))(and (bef in2 in4) (bef in4 in1)))))
(assert (=> (bef in2 in3) (or (soI in2 in3) (matchm in2 in3)(and (bef in2 in1) (bef in1 in3))(and (bef in2 in4) (bef in4 in3)))))
(assert (=> (bef in2 in4) (or (soI in2 in4) (matchm in2 in4)(and (bef in2 in1) (bef in1 in4))(and (bef in2 in3) (bef in3 in4)))))
(assert (=> (bef in3 in1) (or (soI in3 in1) (matchm in3 in1)(and (bef in3 in2) (bef in2 in1))(and (bef in3 in4) (bef in4 in1)))))
(assert (=> (bef in3 in2) (or (soI in3 in2) (matchm in3 in2)(and (bef in3 in1) (bef in1 in2))(and (bef in3 in4) (bef in4 in2)))))
(assert (=> (bef in3 in4) (or (soI in3 in4) (matchm in3 in4)(and (bef in3 in1) (bef in1 in4))(and (bef in3 in2) (bef in2 in4)))))
(assert (=> (bef in4 in1) (or (soI in4 in1) (matchm in4 in1)(and (bef in4 in2) (bef in2 in1))(and (bef in4 in3) (bef in3 in1)))))
(assert (=> (bef in4 in2) (or (soI in4 in2) (matchm in4 in2)(and (bef in4 in1) (bef in1 in2))(and (bef in4 in3) (bef in3 in2)))))
(assert (=> (bef in4 in3) (or (soI in4 in3) (matchm in4 in3)(and (bef in4 in1) (bef in1 in3))(and (bef in4 in2) (bef in2 in3)))))

 ;
 ; (declare-const inv1 I)
 ; (declare-const inv2 I)
 ; (assert (and (= (itype inv1) Deq) (completed inv1) (= (retval inv1) EMPTY)))
 ; (assert (and (= (itype inv2) Enq) (bef inv2 inv1) (completed inv2)))
 ; (assert (forall ((i I)) (not (matchm inv2 i)) ))

;FIFO-1

; (declare-const inv1 I)
; (declare-const inv2 I)
; (declare-const inv3 I)
; (assert (and (= (itype inv1) Enq) (completed inv1) (matchm inv2 inv3) (bef inv1 inv2) ))
; (assert (forall ((i I))  (not (matchm inv1 i)) ) )

;FIFO-2
(declare-const inv1 I)
(declare-const inv2 I)
(declare-const inv3 I)
(declare-const inv4 I)
 (assert (and (matchm inv1 inv4) (matchm inv2 inv3) (bef inv1 inv2) (bef inv3 inv4) ))

;Consistency Policy
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (soE e1 e2) (vis e2 e3)) (vis e1 e3)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (vis e1 e2) (soE e2 e3)) (vis e1 e3)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E) (e4 E)) (=> (and (vis e1 e2) (soE e2 e3) (vis e3 e4)) (vis e1 e4)  )) ) ;WFR

;(assert (forall ((e1 E) (e2 E)) (=>  (soE e1 e2) (vis e1 e2)  )) )


; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D1e i)) (= (itype i) Deq)) (vis e1 (D1e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D2e i)) (= (itype i) Deq)) (vis e1 (D2e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D3e i)) (= (itype i) Deq)) (vis e1 (D3e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D4e i)) (= (itype i) Deq)) (vis e1 (D4e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D5e i)) (= (itype i) Deq)) (vis e1 (D5e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D6e i)) (= (itype i) Deq)) (vis e1 (D6e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (soE e1 e2) (vis e2 (D7e i)) (= (itype i) Deq)) (vis e1 (D7e i)) ) ))

; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D1e i)) (= (itype i) Deq)) (vis e1 (D1e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D2e i)) (= (itype i) Deq)) (vis e1 (D2e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D3e i)) (= (itype i) Deq)) (vis e1 (D3e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D4e i)) (= (itype i) Deq)) (vis e1 (D4e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D5e i)) (= (itype i) Deq)) (vis e1 (D5e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D6e i)) (= (itype i) Deq)) (vis e1 (D6e i)) ) ))
; (assert (forall ((e1 E) (e2 E) (i I)) (=> (and (vis e1 e2) (soE e2 (D7e i)) (= (itype i) Deq)) (vis e1 (D7e i)) ) ))


(check-sat)
(get-model)
