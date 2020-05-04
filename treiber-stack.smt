(declare-sort E)
(declare-datatypes () ((I (in1) (in2) (in3) (in4) (in5) (in6)
  )) )
(declare-sort V)
(declare-sort S)

(declare-datatypes () ((EventType (R) (W) (U))))
(declare-datatypes () ((MethodType (Enq)(Deq) )))
(declare-datatypes () ((FieldType (Default)(Val)(Next) )))
(declare-datatypes () ((StmtType (E1t) (E2t) (E3t) (E4t) (D1t) (D2t) (D3t) (D4t) (Bot) )))


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

(declare-fun E1e (I) E)
(declare-fun E2e (I) E)
(declare-fun E3e (I) E)
(declare-fun E4e (I) E)

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
(declare-fun enqh (I) V)
(declare-fun deqh (I) V)
(declare-fun deqv (I) V)
(declare-fun deqn (I) V)


;Locations
(declare-const head V)

;Values
(declare-const zero V)
;(declare-const one V)
(declare-const EMPTY V)
(declare-const NULL V)


;Last Writer Wins
; (assert (forall ((e1 E)) (exists ((e2 E)) (=> (isR e1) (or (and (rfinit e1) (= (rval e1) (initval (loc e1) (field e1)))) (and (isM e2) (sameloc e1 e2) (vis e2 e1) (= (wval e2) (rval e1)) (rf e2 e1) )) ) ) ))
(assert (forall ((e E)) (=> (isR e) (or (rf (D1e in1) e) (rf (D2e in1) e) (rf (D3e in1) e) (rf (D4e in1) e)
   (rf (D1e in2) e) (rf (D2e in2) e) (rf (D3e in2) e) (rf (D4e in2) e)
(rf (D1e in3) e) (rf (D2e in3) e) (rf (D3e in3) e) (rf (D4e in3) e)
(rf (D1e in4) e) (rf (D2e in4) e) (rf (D3e in4) e) (rf (D4e in4) e)
(rf (D1e in5) e) (rf (D2e in5) e) (rf (D3e in5) e) (rf (D4e in5) e)
(rf (D1e in6) e) (rf (D2e in6) e) (rf (D3e in6) e) (rf (D4e in6) e)
  (rf (E1e in1) e) (rf (E2e in1) e) (rf (E3e in1) e) (rf (E4e in1) e)
    (rf (E1e in2) e) (rf (E2e in2) e) (rf (E3e in2) e) (rf (E4e in2) e)
     (rf (E1e in3) e) (rf (E2e in3) e) (rf (E3e in3) e) (rf (E4e in3) e)
      (rf (E1e in4) e) (rf (E2e in4) e) (rf (E3e in4) e) (rf (E4e in4) e)
   (rf (E1e in5) e) (rf (E2e in5) e) (rf (E3e in5) e) (rf (E4e in5) e)
      (rf (E1e in6) e) (rf (E2e in6) e) (rf (E3e in6) e) (rf (E4e in6) e)
      (rfinit e)) ) ))
(assert (forall ((e E)) (=> (rfinit e) (= (rval e) (initval (loc e) (field e)))) ))
(assert (forall ((e1 E) (e2 E)) (=> (and (vis e1 e2) (sameloc e1 e2) (isM e1) (isR e2) (not (isBot e2)) (not (= (stype e1) Bot)) ) (not (rfinit e2)) ) ))
(assert (forall ((e1 E) (e2 E)) (=> (and (isM e1) (isM e2) (sameloc e1 e2) (not (isBot e1)) (not (isBot e2)) (not (= e1 e2)) ) (or (ar e1 e2) (ar e2 e1))) ))
(assert (forall ((e1 E) (e2 E)) (=>  (ar e1 e2)  (not (ar e2 e1))) ))
(assert (forall ((e1 E) (e2 E)) (=> (ar e1 e2) (and (sameloc e1 e2) (isM e1) (isM e2) ) ) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (ar e1 e2) (ar e2 e3))  (ar e1 e3)) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (isR e1) (rf e2 e1) (vis e3 e1) (not (= (stype e3) Bot)) (not (= e3 e2)) (isM e3) (sameloc e2 e3)) (ar e3 e2) ) ))
(assert (forall ((e1 E) (e2 E)) (=>  (rf e1 e2) (and (vis e1 e2) (not (= (stype e1) Bot))(not (isBot e2)) (= (wval e1) (rval e2)) (sameloc e1 e2) (isM e1) (isR e2) ) )) )
(assert (forall ((e1 E) (e2 E)) (=> (vis e1 e2) (not (vis e2 e1)))))
(assert (forall ((e1 E) (e2 E)) (=> (vis e1 e2) (not (soE e2 e1))) ))

;Alternate Ways to encode last writer wins. IGNORE for now

; (assert (forall ((e1 E) (e2 E) (e3 E) (e4 E) (e5 E) (e6 E)) (=> (and (vis e1 e2) (not (= (stype e1) Bot)) (isM e1) (isR e2) (sameloc e1 e2)
;  (=> (and (not (= e3 e1)) (vis e3 e2)  (not (= (stype e3) Bot)) (isM e3) (sameloc e3 e2)) (ar e3 e1) )
;    (=> (and (not (= e4 e1)) (not (= e4 e3)) (vis e4 e2) (not (= (stype e4) Bot)) (isM e4) (sameloc e4 e2)) (ar e4 e1) )
;   (=> (and (not (= e5 e1)) (not (= e5 e3)) (not (= e5 e4)) (vis e5 e2) (not (= (stype e5) Bot)) (isM e5) (sameloc e5 e2)) (ar e5 e1) )
;   ;(=> (and (not (= e6 e1)) (not (= e6 e3)) (not (= e6 e4)) (not (= e6 e5)) (not (= (stype e6) Bot)) (isM e6) (sameloc e6 e2)) (ar e6 e1) )
;     ) (rf e1 e2) ) ))

; (assert (forall ((e1 E) (e2 E) (e3 E) (e4 E) (e5 E) (e6 E) ) (=> (and (isR e2) (not (isBot e2)) (not (= e1 e2)) (not (= e3 e2)) (not (= e4 e2)) (not (= e5 e2))
;   (=> (and (isM e1) (not (isBot e1)) (vis e1 e2) (sameloc e1 e2)) (not (rf e1 e2)))
; (=> (and (isM e3) (not (isBot e3)) (sameloc e3 e2) (vis e3 e2)   (not (= e3 e1))) (not (rf e3 e2)))
; (=> (and (isM e4) (not (isBot e4)) (sameloc e4 e2) (vis e4 e2)  (not (= e4 e1)) (not (= e4 e3))) (not (rf e4 e2)))
; (=> (and (isM e5) (not (isBot e5)) (sameloc e5 e2) (vis e5 e2)    (not (= e5 e1)) (not (= e5 e3)) (not (= e5 e4))) (not (rf e5 e2)) )
;   ) (rfinit e2) ) ))

; (assert (forall ((e2 E)) (exists ((e1 E)) (=> (and (not (isBot e2)) (isR e2) (=> (and (not (isBot e1)) (isM e1) (sameloc e1 e2) ) (not (rf e1 e2))) )
;   (rfinit e2)) ) ))
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
(ite (= (itype i) Enq) (E4e i)
 (ite (not (isBot (D4e i))) (D4e i) (D1e i)  )   )
)
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i2) Enq) (soI i1 i2)) (soE (lastevent i1) (E1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i2) Deq) (soI i1 i2)) (soE (lastevent i1) (D1e i2)))))


(assert (not (= zero EMPTY)))

(assert (= (initval head Default) NULL ))
(assert (forall ((l V)) (= (initval l Next) NULL) ))
(assert (forall ((l V)) (= (initval l Val) zero) ))


(assert (forall((i I)) (not (= (argval i) EMPTY))))
(assert (forall((i I)) (not (= (argval i) zero))))
(assert (forall((i I)) (not (= (newloc i) NULL))))
(assert (forall ((i1 I) (i2 I)) (=> (= (newloc i1) (newloc i2)) (= i1 i2) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D1e i)) (deqh i)) (= (stype (D1e i)) D1t) (= (loc (D1e i)) head) (= (field (D1e i)) Default) (= (etype (D1e i)) R)) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqh i) NULL)) (and (= (retval i) EMPTY) (isBot (D2e i)) (isBot (D3e i)) (isBot (D4e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (deqh i) NULL))) (and (= (rval (D2e i)) (deqv i)) (= (stype (D2e i)) D2t) (= (loc (D2e i)) (deqh i)) (= (field (D2e i)) Val) (= (etype (D2e i)) R) (soE (D1e i) (D2e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (deqh i) NULL))) (and (= (rval (D3e i)) (deqn i)) (= (stype (D3e i)) D3t) (= (loc (D3e i)) (deqh i)) (= (field (D3e i)) Next) (= (etype (D3e i)) R) (soE (D2e i) (D3e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (deqh i) NULL))) (and (= (retval i) (deqv i)) (= (rval (D4e i)) (deqh i)) (= (wval (D4e i)) (deqn i)) (= (stype (D4e i)) D4t) (= (loc (D4e i)) head) (= (field (D4e i)) Default) (= (etype (D4e i)) U) (soE (D3e i) (D4e i)) ) ) ))


(declare-const botevent E)
(assert (= (stype botevent) Bot ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (= (D1e i) botevent) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (D2e i) botevent) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (D3e i) botevent) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (D4e i) botevent) )))

(assert (forall ((i I)) (=> (= (itype i) Deq) (= (E1e i) botevent) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (E2e i) botevent) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (E3e i) botevent) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (E4e i) botevent) )))

(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E1e i)) W)  (= (loc (E1e i)) (newloc i)) (= (stype (E1e i)) E1t) (= (field (E1e i)) Val) (= (wval (E1e i)) (argval i)) ) ) ))

(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E1e i) (E2e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E2e i)) R)  (= (loc (E2e i)) head) (= (stype (E2e i)) E2t) (= (field (E2e i)) Default) (= (rval (E2e i)) (enqh i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E2e i) (E3e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E3e i)) W)  (= (loc (E3e i)) (newloc i)) (= (stype (E3e i)) E3t) (= (field (E3e i)) Next) (= (wval (E3e i)) (enqh i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E3e i) (E4e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E4e i)) U)  (= (loc (E4e i)) head) (= (stype (E4e i)) E4t) (= (field (E4e i)) Default) (= (rval (E4e i)) (enqh i)) (= (wval (E4e i)) (newloc i) ) ) ) ))


(assert (forall ((i1 I) (i2 I)) (=> (and (= (D1e i1) (D1e i2)) (not (= (stype (D1e i1)) Bot))) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (D2e i1) (D2e i2)) (not (= (stype (D2e i1)) Bot))) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (D3e i1) (D3e i2)) (not (= (stype (D3e i1)) Bot))) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (D4e i1) (D4e i2)) (not (= (stype (D4e i1)) Bot))) (= i1 i2) ) ))

(assert (forall ((i1 I) (i2 I)) (=> (and (= (E1e i1) (E1e i2)) (not (= (stype (E1e i1)) Bot))) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (E2e i1) (E2e i2)) (not (= (stype (E2e i1)) Bot))) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (E3e i1) (E3e i2)) (not (= (stype (E3e i1)) Bot))) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (E4e i1) (E4e i2)) (not (= (stype (E4e i1)) Bot))) (= i1 i2) ) ))

;Stack Specification
(declare-fun matchm (I I) Bool)
(assert (forall ((i1 I) (i2 I)) (= (matchm i1 i2) (and (= (itype i1) Enq) (= (itype i2) Deq) (= (argval i1) (retval i2)))) ))
(assert (forall ((i1 I) (i2 I)) (=> (not (= i1 i2))(not (= (argval i1) (argval i2)))) ))

;AddRem
; (assert (= (itype in1) Deq))
; (assert (forall ((i I)) (and (not (= (retval in1) EMPTY)) (not (matchm i in1)) ) ))


;Injective
;(assert (matchm in1 in2))
;(assert (matchm in1 in3))

;Empty
(declare-fun bef (I I) Bool)

 (assert (forall ((i1 I) (i2 I)) (=>  (bef i1 i2)  (not (bef i2 i1))) ))
 (assert (forall ((e1 I) (e2 I) (e3 I)) (=> (and (bef e1 e2) (bef e2 e3))  (bef e1 e3)) ))
; ;
  (assert (forall ((i1 I) (i2 I)) (=>  (matchm i1 i2)  (bef i1 i2))) )
  (assert (forall ((i1 I) (i2 I)) (=> (soI i1 i2)  (bef i1 i2))) )

;(assert (forall ((i1 I) (i2 I)) (exists ((i3 I)) (=> (bef i1 i2) (or (soI i1 i2) (matchm i1 i2) (and (bef i1 i3) (bef i3 i2))))   ) ))
(assert (=> (bef in1 in2) (or (soI in1 in2) (matchm in1 in2)(and (bef in1 in3) (bef in3 in2))(and (bef in1 in4) (bef in4 in2))(and (bef in1 in5) (bef in5 in2))(and (bef in1 in6) (bef in6 in2)))))
(assert (=> (bef in1 in3) (or (soI in1 in3) (matchm in1 in3)(and (bef in1 in2) (bef in2 in3))(and (bef in1 in4) (bef in4 in3))(and (bef in1 in5) (bef in5 in3))(and (bef in1 in6) (bef in6 in3)))))
(assert (=> (bef in1 in4) (or (soI in1 in4) (matchm in1 in4)(and (bef in1 in2) (bef in2 in4))(and (bef in1 in3) (bef in3 in4))(and (bef in1 in5) (bef in5 in4))(and (bef in1 in6) (bef in6 in4)))))
(assert (=> (bef in1 in5) (or (soI in1 in5) (matchm in1 in5)(and (bef in1 in2) (bef in2 in5))(and (bef in1 in3) (bef in3 in5))(and (bef in1 in4) (bef in4 in5))(and (bef in1 in6) (bef in6 in5)))))
(assert (=> (bef in1 in6) (or (soI in1 in6) (matchm in1 in6)(and (bef in1 in2) (bef in2 in6))(and (bef in1 in3) (bef in3 in6))(and (bef in1 in4) (bef in4 in6))(and (bef in1 in5) (bef in5 in6)))))
(assert (=> (bef in2 in1) (or (soI in2 in1) (matchm in2 in1)(and (bef in2 in3) (bef in3 in1))(and (bef in2 in4) (bef in4 in1))(and (bef in2 in5) (bef in5 in1))(and (bef in2 in6) (bef in6 in1)))))
(assert (=> (bef in2 in3) (or (soI in2 in3) (matchm in2 in3)(and (bef in2 in1) (bef in1 in3))(and (bef in2 in4) (bef in4 in3))(and (bef in2 in5) (bef in5 in3))(and (bef in2 in6) (bef in6 in3)))))
(assert (=> (bef in2 in4) (or (soI in2 in4) (matchm in2 in4)(and (bef in2 in1) (bef in1 in4))(and (bef in2 in3) (bef in3 in4))(and (bef in2 in5) (bef in5 in4))(and (bef in2 in6) (bef in6 in4)))))
(assert (=> (bef in2 in5) (or (soI in2 in5) (matchm in2 in5)(and (bef in2 in1) (bef in1 in5))(and (bef in2 in3) (bef in3 in5))(and (bef in2 in4) (bef in4 in5))(and (bef in2 in6) (bef in6 in5)))))
(assert (=> (bef in2 in6) (or (soI in2 in6) (matchm in2 in6)(and (bef in2 in1) (bef in1 in6))(and (bef in2 in3) (bef in3 in6))(and (bef in2 in4) (bef in4 in6))(and (bef in2 in5) (bef in5 in6)))))
(assert (=> (bef in3 in1) (or (soI in3 in1) (matchm in3 in1)(and (bef in3 in2) (bef in2 in1))(and (bef in3 in4) (bef in4 in1))(and (bef in3 in5) (bef in5 in1))(and (bef in3 in6) (bef in6 in1)))))
(assert (=> (bef in3 in2) (or (soI in3 in2) (matchm in3 in2)(and (bef in3 in1) (bef in1 in2))(and (bef in3 in4) (bef in4 in2))(and (bef in3 in5) (bef in5 in2))(and (bef in3 in6) (bef in6 in2)))))
(assert (=> (bef in3 in4) (or (soI in3 in4) (matchm in3 in4)(and (bef in3 in1) (bef in1 in4))(and (bef in3 in2) (bef in2 in4))(and (bef in3 in5) (bef in5 in4))(and (bef in3 in6) (bef in6 in4)))))
(assert (=> (bef in3 in5) (or (soI in3 in5) (matchm in3 in5)(and (bef in3 in1) (bef in1 in5))(and (bef in3 in2) (bef in2 in5))(and (bef in3 in4) (bef in4 in5))(and (bef in3 in6) (bef in6 in5)))))
(assert (=> (bef in3 in6) (or (soI in3 in6) (matchm in3 in6)(and (bef in3 in1) (bef in1 in6))(and (bef in3 in2) (bef in2 in6))(and (bef in3 in4) (bef in4 in6))(and (bef in3 in5) (bef in5 in6)))))
(assert (=> (bef in4 in1) (or (soI in4 in1) (matchm in4 in1)(and (bef in4 in2) (bef in2 in1))(and (bef in4 in3) (bef in3 in1))(and (bef in4 in5) (bef in5 in1))(and (bef in4 in6) (bef in6 in1)))))
(assert (=> (bef in4 in2) (or (soI in4 in2) (matchm in4 in2)(and (bef in4 in1) (bef in1 in2))(and (bef in4 in3) (bef in3 in2))(and (bef in4 in5) (bef in5 in2))(and (bef in4 in6) (bef in6 in2)))))
(assert (=> (bef in4 in3) (or (soI in4 in3) (matchm in4 in3)(and (bef in4 in1) (bef in1 in3))(and (bef in4 in2) (bef in2 in3))(and (bef in4 in5) (bef in5 in3))(and (bef in4 in6) (bef in6 in3)))))
(assert (=> (bef in4 in5) (or (soI in4 in5) (matchm in4 in5)(and (bef in4 in1) (bef in1 in5))(and (bef in4 in2) (bef in2 in5))(and (bef in4 in3) (bef in3 in5))(and (bef in4 in6) (bef in6 in5)))))
(assert (=> (bef in4 in6) (or (soI in4 in6) (matchm in4 in6)(and (bef in4 in1) (bef in1 in6))(and (bef in4 in2) (bef in2 in6))(and (bef in4 in3) (bef in3 in6))(and (bef in4 in5) (bef in5 in6)))))
(assert (=> (bef in5 in1) (or (soI in5 in1) (matchm in5 in1)(and (bef in5 in2) (bef in2 in1))(and (bef in5 in3) (bef in3 in1))(and (bef in5 in4) (bef in4 in1))(and (bef in5 in6) (bef in6 in1)))))
(assert (=> (bef in5 in2) (or (soI in5 in2) (matchm in5 in2)(and (bef in5 in1) (bef in1 in2))(and (bef in5 in3) (bef in3 in2))(and (bef in5 in4) (bef in4 in2))(and (bef in5 in6) (bef in6 in2)))))
(assert (=> (bef in5 in3) (or (soI in5 in3) (matchm in5 in3)(and (bef in5 in1) (bef in1 in3))(and (bef in5 in2) (bef in2 in3))(and (bef in5 in4) (bef in4 in3))(and (bef in5 in6) (bef in6 in3)))))
(assert (=> (bef in5 in4) (or (soI in5 in4) (matchm in5 in4)(and (bef in5 in1) (bef in1 in4))(and (bef in5 in2) (bef in2 in4))(and (bef in5 in3) (bef in3 in4))(and (bef in5 in6) (bef in6 in4)))))
(assert (=> (bef in5 in6) (or (soI in5 in6) (matchm in5 in6)(and (bef in5 in1) (bef in1 in6))(and (bef in5 in2) (bef in2 in6))(and (bef in5 in3) (bef in3 in6))(and (bef in5 in4) (bef in4 in6)))))
(assert (=> (bef in6 in1) (or (soI in6 in1) (matchm in6 in1)(and (bef in6 in2) (bef in2 in1))(and (bef in6 in3) (bef in3 in1))(and (bef in6 in4) (bef in4 in1))(and (bef in6 in5) (bef in5 in1)))))
(assert (=> (bef in6 in2) (or (soI in6 in2) (matchm in6 in2)(and (bef in6 in1) (bef in1 in2))(and (bef in6 in3) (bef in3 in2))(and (bef in6 in4) (bef in4 in2))(and (bef in6 in5) (bef in5 in2)))))
(assert (=> (bef in6 in3) (or (soI in6 in3) (matchm in6 in3)(and (bef in6 in1) (bef in1 in3))(and (bef in6 in2) (bef in2 in3))(and (bef in6 in4) (bef in4 in3))(and (bef in6 in5) (bef in5 in3)))))
(assert (=> (bef in6 in4) (or (soI in6 in4) (matchm in6 in4)(and (bef in6 in1) (bef in1 in4))(and (bef in6 in2) (bef in2 in4))(and (bef in6 in3) (bef in3 in4))(and (bef in6 in5) (bef in5 in4)))))
(assert (=> (bef in6 in5) (or (soI in6 in5) (matchm in6 in5)(and (bef in6 in1) (bef in1 in5))(and (bef in6 in2) (bef in2 in5))(and (bef in6 in3) (bef in3 in5))(and (bef in6 in4) (bef in4 in5)))))

 ; (declare-const inv1 I)
 ;  (declare-const inv2 I)
 ;  (assert (and (= (itype inv1) Deq) (= (retval inv1) EMPTY)))
 ;  (assert (and (= (itype inv2) Enq) (bef inv2 inv1)))
 ;  (assert (forall ((i I)) (not (matchm inv2 i)) ))

;LIFO-1

 ; (declare-const inv1 I)
 ; (declare-const inv2 I)
 ; (declare-const inv3 I)
 ; (assert (and (= (itype inv1) Enq) (matchm inv2 inv3) (bef inv2 inv1) (bef inv1 inv3) ))
 ; (assert (forall ((i I)) (not (matchm inv1 i)) ))

;LIFO-2

(declare-const inv1 I)
(declare-const inv2 I)
(declare-const inv3 I)
(declare-const inv4 I)
(assert (and (matchm inv1 inv4) (matchm inv2 inv3) (bef inv2 inv1) (bef inv3 inv4) (bef inv1 inv3) ))

;Consistency Policy
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (soE e1 e2) (vis e2 e3)) (vis e1 e3)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (vis e1 e2) (soE e2 e3)) (vis e1 e3)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E) (e4 E)) (=> (and (vis e1 e2) (soE e2 e3) (vis e3 e4)) (vis e1 e4)  )) ) ;WFR
;(assert (forall ((e1 E) (e2 E)) (=>  (soE e1 e2) (vis e1 e2)  )) )

;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (tot e1 e2) (vis e2 e3)) (vis e1 e3)  )) ) ;CV
;(assert (forall ((e1 E) (e2 E)) (=>  (tot e1 e2) (vis e1 e2)  )) ) ;CC


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
