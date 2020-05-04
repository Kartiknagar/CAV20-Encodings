(declare-sort E)
(declare-datatypes () ((I (in1) (in2) (in3) (in4);(in5) ;(in6)
  )) )
;(declare-sort V)
(declare-sort S)

(declare-datatypes () ((EventType (R) (W) (U))))
(declare-datatypes () ((MethodType (Enq)(Deq) )))
(declare-datatypes () ((FieldType (Default)(Val) )))
(declare-datatypes () ((StmtType (E1t) (E2t) (E3t) (D1t) (D21t) (D22t) (D23t) (D24t) (D25t) (Bot) )))


(declare-fun etype (E) EventType)
(declare-fun stype (E) StmtType)
(declare-fun loc (E) Int)
(declare-fun field (E) FieldType)
(declare-fun rval (E) Int)
(declare-fun wval (E) Int)
(declare-fun vis (E E) Bool)
(declare-fun ar (E E) Bool)
(declare-fun soE (E E) Bool)
(declare-fun sameInv (E E) Bool)

(declare-fun rf (E E) Bool)

(declare-fun soI (I I) Bool)
(declare-fun sess (I) S)
(declare-fun argval (I) Int)
(declare-fun retval (I) Int)
(declare-fun itype (I) MethodType)
(declare-fun completed (I) Bool)

(declare-fun D1e (I) E)
(declare-fun D21e (I) E)
(declare-fun D22e (I) E)
(declare-fun D23e (I) E)
(declare-fun D24e (I) E)
(declare-fun D25e (I) E)

(declare-fun E1e (I) E)
(declare-fun E2e (I) E)
(declare-fun E3e (I) E)

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

(declare-fun initval (Int FieldType) Int)
(declare-fun rfinit (E) Bool)


;Local Variables
(declare-fun enqb (I) Int)
(declare-fun deqr (I) Int)
(declare-fun deqt1 (I) Int)
(declare-fun deqt2 (I) Int)
(declare-fun deqt3 (I) Int)
(declare-fun deqt4 (I) Int)
(declare-fun deqt5 (I) Int)


;Locations
(declare-const tail Int)

;Values
;(declare-const one V)
(declare-const EMPTY Int)
(declare-const NULL Int)


;Last Writer Wins
;(assert (forall ((e1 E)) (exists ((e2 E)) (=> (isR e1) (or (and (rfinit e1) (= (rval e1) (initval (loc e1) (field e1)))) (and (isM e2) (sameloc e1 e2) (vis e2 e1) (= (wval e2) (rval e1)) (rf e2 e1) )) ) ) ))
(assert (forall ((e E)) (=> (isR e) (or (rf (D1e in1) e) (rf (D21e in1) e) (rf (D22e in1) e) (rf (D23e in1) e) (rf (D24e in1) e) (rf (D25e in1) e)
(rf (D1e in2) e) (rf (D21e in2) e) (rf (D22e in2) e) (rf (D23e in2) e) (rf (D24e in2) e) (rf (D25e in2) e)
(rf (D1e in3) e) (rf (D21e in3) e) (rf (D22e in3) e) (rf (D23e in3) e) (rf (D24e in3) e) (rf (D25e in3) e)
(rf (D1e in4) e) (rf (D21e in4) e) (rf (D22e in4) e) (rf (D23e in4) e) (rf (D24e in4) e) (rf (D25e in4) e)
;(rf (D1e in5) e) (rf (D21e in5) e) (rf (D22e in5) e) (rf (D23e in5) e) (rf (D24e in5) e) (rf (D25e in5) e)
;(rf (D1e in6) e) (rf (D21e in6) e) (rf (D22e in6) e) (rf (D23e in6) e) (rf (D24e in6) e) (rf (D25e in6) e)
  (rf (E1e in1) e) (rf (E2e in1) e) (rf (E3e in1) e)
    (rf (E1e in2) e) (rf (E2e in2) e) (rf (E3e in2) e)
     (rf (E1e in3) e) (rf (E2e in3) e) (rf (E3e in3) e)
      (rf (E1e in4) e) (rf (E2e in4) e) (rf (E3e in4) e)
; (rf (E1e in5) e) (rf (E2e in5) e) (rf (E3e in5) e)
;      (rf (E1e in6) e) (rf (E2e in6) e) (rf (E3e in6) e)
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

;Same Invocation relation
(assert (forall ((e1 E) (e2 E)) (=>  (sameInv e1 e2) (sameInv e2 e1) ) ))
(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (sameInv e1 e2) (sameInv e2 e3))  (sameInv e1 e3)) ))


(define-fun lastevent ((i I)) E
(ite (= (itype i) Enq) (E3e i)
 (ite (not (isBot (D25e i))) (D25e i) (ite (not (isBot (D24e i))) (D24e i) (ite (not (isBot (D23e i))) (D23e i)
(ite (not (isBot (D22e i))) (D22e i) (ite (not (isBot (D21e i))) (D21e i) (D1e i) ) )  ) )  )   )
)
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i2) Enq) (soI i1 i2)) (soE (lastevent i1) (E1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i2) Deq) (soI i1 i2)) (soE (lastevent i1) (D1e i2)))))


(assert (not (= 0 EMPTY)))
(assert (not (= 0 NULL)))
(assert (not (= NULL EMPTY)))

(assert (= (initval tail Default) 0 ))
(assert (forall ((l Int)) (= (initval l Val) NULL) ))


(assert (forall((i I)) (not (= (argval i) EMPTY))))
(assert (forall((i I)) (not (= (argval i) 0))))
(assert (forall((i I)) (not (= (argval i) NULL))))
;(assert (forall((i I)) (not (= (newloc i) NULL))))


(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (- (rval (D1e i)) 1) (deqr i)) (= (stype (D1e i)) D1t) (= (loc (D1e i)) tail) (= (field (D1e i)) Default) (= (etype (D1e i)) R)) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (< (deqr i) 0)) (and (= (retval i) EMPTY) (completed i) (isBot (D21e i)) (isBot (D22e i)) (isBot (D23e i)) (isBot (D24e i)) (isBot (D25e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 0)) (and (= (rval (D21e i)) (deqt1 i)) (= (stype (D21e i)) D21t) (= (loc (D21e i)) 0) (= (field (D21e i)) Val) (isR (D21e i)) (soE (D1e i) (D21e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 0) (not (= (deqt1 i) NULL)) ) (and (completed i) (= (retval i) (deqt1 i))
(isBot (D22e i)) (isBot (D23e i)) (isBot (D24e i)) (isBot (D25e i)) (= (etype (D21e i)) U) (= (wval (D21e i)) NULL)  ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 0)  (= (deqt1 i) NULL) ) (= (etype (D21e i)) R) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqr i) 0) (= (deqt1 i) NULL) ) (and (completed i) (= (retval i) EMPTY)
(isBot (D22e i)) (isBot (D23e i)) (isBot (D24e i)) (isBot (D25e i))  ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 1)  (= (deqt1 i) NULL) ) (and (= (rval (D22e i)) (deqt2 i)) (= (stype (D22e i)) D22t) (= (loc (D22e i)) 1) (= (field (D22e i)) Val) (isR (D22e i)) (soE (D21e i) (D22e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 1) (= (deqt1 i) NULL) (not (= (deqt2 i) NULL)) ) (and (completed i) (= (retval i) (deqt2 i)) (isBot (D23e i)) (isBot (D24e i)) (isBot (D25e i)) (= (etype (D22e i)) U) (= (wval (D22e i)) NULL)  ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 1) (= (deqt1 i) NULL)  (= (deqt2 i) NULL) ) (= (etype (D22e i)) R ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqr i) 1) (= (deqt1 i) NULL) (= (deqt2 i) NULL) ) (and (completed i) (= (retval i) EMPTY)
(isBot (D23e i)) (isBot (D24e i)) (isBot (D25e i))  ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 2)  (= (deqt1 i) NULL) (= (deqt2 i) NULL) ) (and (= (rval (D23e i)) (deqt3 i)) (= (stype (D23e i)) D23t) (= (loc (D23e i)) 2) (= (field (D23e i)) Val) (isR (D23e i)) (soE (D22e i) (D23e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 2) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (not (= (deqt3 i) NULL)) ) (and (completed i) (= (retval i) (deqt3 i)) (isBot (D24e i)) (isBot (D25e i)) (= (etype (D23e i)) U) (= (wval (D23e i)) NULL) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 2) (= (deqt1 i) NULL) (= (deqt2 i) NULL)  (= (deqt3 i) NULL) ) (= (etype (D23e i)) R) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqr i) 2) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) ) (and (completed i) (= (retval i) EMPTY) (isBot (D24e i)) (isBot (D25e i))  ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 3)  (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) ) (and (= (rval (D24e i)) (deqt4 i)) (= (stype (D24e i)) D24t) (= (loc (D24e i)) 3) (= (field (D24e i)) Val) (isR (D24e i)) (soE (D23e i) (D24e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 3) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (not (= (deqt4 i) NULL)) ) (and (completed i) (= (retval i) (deqt4 i))  (isBot (D25e i)) (= (etype (D24e i)) U) (= (wval (D24e i)) NULL) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 3) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL) ) (= (etype (D24e i)) R) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqr i) 3) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL) ) (and (completed i) (= (retval i) EMPTY) (isBot (D25e i))  ) ) ))


(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 4)  (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL) ) (and (= (rval (D25e i)) (deqt5 i)) (= (stype (D25e i)) D25t) (= (loc (D25e i)) 4) (= (field (D25e i)) Val) (isR (D25e i)) (soE (D24e i) (D25e i)) ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 4) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL) (not (= (deqt5 i) NULL)) ) (and (completed i) (= (retval i) (deqt4 i)) (= (etype (D25e i)) U) (= (wval (D25e i)) NULL)  ) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 4) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL)  (= (deqt5 i) NULL) ) (= (etype (D25e i)) R) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (>= (deqr i) 4) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL) (= (deqt5 i) NULL) ) (not (completed i)) ) ))

(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqr i) 4) (= (deqt1 i) NULL) (= (deqt2 i) NULL) (= (deqt3 i) NULL) (= (deqt4 i) NULL) (= (deqt5 i) NULL) ) (and (completed i) (= (retval i) EMPTY) ) ) ))

;(assert (forall ((i I)) (=> (= (itype i) Deq) (and (sameInv (D1e i) (D21e i)) (sameInv (D21e i) (D22e i)) (sameInv (D22e i) (D23e i)) (sameInv (D23e i) (D24e i)) (sameInv (D24e i) (D25e i))  ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D1e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D21e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D22e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D23e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D24e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D25e i)) Bot ) )))

(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E1e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E2e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E3e i)) Bot ) )))

(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E1e i)) R)  (= (loc (E1e i)) tail) (= (stype (E1e i)) E1t) (= (field (E1e i)) Default) (= (rval (E1e i)) (enqb i)) ) ) ))

(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E1e i) (E2e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E2e i)) U)  (= (loc (E2e i)) tail) (= (stype (E2e i)) E2t) (= (field (E2e i)) Default) (= (rval (E2e i)) (enqb i)) (= (wval (E2e i)) (+ (enqb i) 1) ) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E2e i) (E3e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E3e i)) W)  (= (loc (E3e i)) (enqb i)) (= (stype (E3e i)) E3t) (= (field (E3e i)) Val) (= (wval (E3e i)) (argval i)) ) ) ))

;(assert (forall ((i I)) (=> (= (itype i) Enq) (and (sameInv (E1e i) (E2e i)) (sameInv (E2e i) (E3e i)) ) ) ))

; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E1t) (= (stype e2) D1t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E1t) (= (stype e2) D21t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E1t) (= (stype e2) D22t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E1t) (= (stype e2) D23t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E1t) (= (stype e2) D24t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E1t) (= (stype e2) D25t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E2t) (= (stype e2) D1t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E2t) (= (stype e2) D21t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E2t) (= (stype e2) D22t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E2t) (= (stype e2) D23t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E2t) (= (stype e2) D24t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E2t) (= (stype e2) D25t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E3t) (= (stype e2) D1t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E3t) (= (stype e2) D21t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E3t) (= (stype e2) D22t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E3t) (= (stype e2) D23t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E3t) (= (stype e2) D24t)) (not (sameInv e1 e2)) ) ))
; (assert (forall ((e1 E) (e2 E)) (=> (and (= (stype e1) E3t) (= (stype e2) D25t)) (not (sameInv e1 e2)) ) ))


(assert (forall ((i1 I) (i2 I)) (=> (= (D1e i1) (D1e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D21e i1) (D21e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D22e i1) (D22e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D23e i1) (D23e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D24e i1) (D24e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (D25e i1) (D25e i2)) (= i1 i2) ) ))

(assert (forall ((i1 I) (i2 I)) (=> (= (E1e i1) (E1e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E2e i1) (E2e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (E3e i1) (E3e i2)) (= i1 i2) ) ))

;Queue Specification
(declare-fun matchm (I I) Bool)
(assert (forall ((i1 I) (i2 I)) (= (matchm i1 i2) (and (= (itype i1) Enq) (= (itype i2) Deq) (completed i2) (= (argval i1) (retval i2)))) ))
(assert (forall ((i1 I) (i2 I)) (=> (not (= i1 i2))(not (= (argval i1) (argval i2)))) ))

;AddRem
;(assert (= (itype in1) Deq))
;(assert (forall ((i I)) (and (not (= (retval in1) EMPTY)) (completed in1) (not (matchm i in1)) ) ))


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


; (declare-const inv1 I)
;  (declare-const inv2 I)
;  (assert (and (= (itype inv1) Deq) (completed inv1) (= (retval inv1) EMPTY)))
;  (assert (and (= (itype inv2) Enq) (bef inv2 inv1)))
;  (assert (forall ((i I)) (not (matchm inv2 i)) ))

;FIFO-1

 ; (declare-const inv1 I)
 ; (declare-const inv2 I)
 ; (declare-const inv3 I)
 ; (assert (and (= (itype inv1) Enq) (matchm inv2 inv3) (bef inv1 inv2) ))
 ; (assert (forall ((i I)) (not (matchm inv1 i) ) ))

 ;FIFO-2
 (declare-const inv1 I)
 (declare-const inv2 I)
 (declare-const inv3 I)
 (declare-const inv4 I)
  (assert (and (matchm inv1 inv4) (matchm inv2 inv3) (bef inv1 inv2) (bef inv3 inv4) ))

;Consistency Policy
 (assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (soE e1 e2) (vis e2 e3)) (vis e1 e3)  )) )
 (assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (vis e1 e2) (soE e2 e3)) (vis e1 e3)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E) (e4 E)) (=> (and (vis e1 e2) (soE e2 e3) (vis e3 e4)) (vis e1 e4)  )) ) ;WFR
;(assert (forall ((e1 E) (e2 E)) (=>  (soE e1 e2) (vis e1 e2)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (sameInv e1 e2) (not (sameInv e2 e3)) (vis e2 e3)) (vis e1 e3)  )) )

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
