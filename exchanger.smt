(declare-sort E)
(declare-datatypes () ((I (in1) (in2) (in3) (in4)
  )) )
(declare-sort V)
(declare-sort S)

(declare-datatypes () ((EventType (R) (W) (U))))
(declare-datatypes () ((MethodType (Exch) )))
(declare-datatypes () ((FieldType (Default)(First)(Second) )))
(declare-datatypes () ((StmtType (S1t) (S2t) (S3t) (S4t) (S5t) (S6t) (S7t) (Bot) )))


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

(declare-fun S1e (I) E)
(declare-fun S2e (I) E)
(declare-fun S3e (I) E)
(declare-fun S4e (I) E)
(declare-fun S5e (I) E)
(declare-fun S6e (I) E)
(declare-fun S7e (I) E)

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
(declare-fun ls (I) V)


;Locations
(declare-const slot V)

;Values
(declare-const zero V)
;(declare-const one V)
(declare-const BOTVAL V)
(declare-const START V)


;Last Writer Wins
;(assert (forall ((e1 E)) (exists ((e2 E)) (=> (isR e1) (or (and (rfinit e1) (= (rval e1) (initval (loc e1) (field e1)))) (and (isM e2) (sameloc e1 e2) (vis e2 e1) (= (wval e2) (rval e1)) (rf e2 e1) )) ) ) ))
(assert (forall ((e E)) (=> (isR e) (or (rf (S1e in1) e) (rf (S2e in1) e) (rf (S3e in1) e) (rf (S4e in1) e) (rf (S5e in1) e) (rf (S6e in1) e) (rf (S7e in1) e) (rf (S1e in2) e) (rf (S2e in2) e) (rf (S3e in2) e) (rf (S4e in2) e) (rf (S5e in2) e) (rf (S6e in2) e) (rf (S7e in2) e)
(rf (S1e in3) e) (rf (S2e in3) e) (rf (S3e in3) e) (rf (S4e in3) e) (rf (S5e in3) e) (rf (S6e in3) e) (rf (S7e in3) e)
(rf (S1e in4) e) (rf (S2e in4) e) (rf (S3e in4) e) (rf (S4e in4) e) (rf (S5e in4) e) (rf (S6e in4) e) (rf (S7e in4) e)
;(rf (D1e in3) e) (rf (D2e in3) e) (rf (D3e in3) e) (rf (D4e in3) e) (rf (D5e in3) e) (rf (D6e in3) e) (rf (D7e in3) e)
;(rf (D1e in4) e) (rf (D2e in4) e) (rf (D3e in4) e) (rf (D4e in4) e) (rf (D5e in4) e) (rf (D6e in4) e) (rf (D7e in4) e)
  ;(rf (E1e in1) e) (rf (E2e in1) e) (rf (E3e in1) e) (rf (E4e in1) e) (rf (E5e in1) e) (rf (E6e in1) e) (rf (E7e in1) e)
  ;  (rf (E1e in2) e) (rf (E2e in2) e) (rf (E3e in2) e) (rf (E4e in2) e) (rf (E5e in2) e) (rf (E6e in2) e) (rf (E7e in2) e)
;      (rf (E1e in3) e) (rf (E2e in3) e) (rf (E3e in3) e) (rf (E4e in3) e) (rf (E5e in3) e) (rf (E6e in3) e) (rf (E7e in3) e)
;      (rf (E1e in4) e) (rf (E2e in4) e) (rf (E3e in4) e) (rf (E4e in4) e) (rf (E5e in4) e) (rf (E6e in4) e) (rf (E7e in4) e)
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
 (ite (not (isBot (S7e i))) (S7e i) (ite (not (isBot (S6e i))) (S6e i) (ite (not (isBot (S4e i))) (S4e i) (S3e i)  ) ) )
)
(assert (forall ((i1 I) (i2 I)) (=>  (soI i1 i2) (soE (lastevent i1) (S1e i2)))))

(assert (not (= zero BOTVAL)))


(assert (= (initval slot Default) START ))
(assert (forall ((l V)) (= (initval l First) zero) ))
(assert (forall ((l V)) (= (initval l Second) zero) ))


(assert (forall((i I)) (not (= (argval i) BOTVAL))))
(assert (forall((i I)) (not (= (argval i) zero))))
(assert (forall((i I)) (not (= (newloc i) BOTVAL))))
(assert (forall((i I)) (not (= (newloc i) zero))))
(assert (forall((i I)) (not (= (newloc i) slot))))
(assert (forall((i I)) (not (= (newloc i) START))))
(assert (forall ((i1 I) (i2 I)) (=> (= (newloc i1) (newloc i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (not (= i1 i2))(not (= (argval i1) (argval i2)))) ))



(assert (forall ((i I)) (and (= (etype (S1e i)) R)  (= (loc (S1e i)) slot) (= (stype (S1e i)) S1t) (= (field (S1e i)) Default) (= (rval (S1e i)) (ls i)) ) ))

(assert (forall ((i I))  (soE (S1e i) (S2e i)) ) )
(assert (forall ((i I))  (and (isR (S2e i)) (= (loc (S2e i)) (ls i)) (= (stype (S2e i)) S2t) (= (field (S2e i)) First)  ) ) )
(assert (forall ((i I)) (=> (= (rval (S2e i)) zero) (and (= (etype (S2e i)) U) (= (wval (S2e i)) (argval i) ) ) ) ))
(assert (forall ((i I)) (=> (not (= (rval (S2e i)) zero)) (= (etype (S2e i)) R)  ) ))

(assert (forall ((i I)) (=> (= (rval (S2e i)) zero) (and (isBot (S5e i)) (isBot (S6e i)) (isBot (S7e i))) ) ))

(assert (forall ((i I)) (=> (= (rval (S2e i)) zero) (and (isR (S3e i)) (= (loc (S3e i)) (ls i)) (= (stype (S3e i)) S3t) (= (field (S3e i)) Second) (soE (S2e i) (S3e i)) ) ) ))
(assert (forall ((i I)) (=> (and (= (rval (S2e i)) zero) (= (rval (S3e i)) zero)) (and (= (etype (S3e i)) U) (= (wval (S3e i)) BOTVAL )
(= (retval i) BOTVAL) (isBot (S4e i)) ) ) ))
(assert (forall ((i I)) (=> (and (= (rval (S2e i)) zero) (not (= (rval (S3e i)) zero))) (= (etype (S3e i)) R)  ) ))

(assert (forall ((i I)) (=> (and (= (rval (S2e i)) zero) (not (= (rval (S3e i)) zero))) (and (= (etype (S4e i)) R)  (= (loc (S4e i)) (ls i)) (= (stype (S4e i)) S4t) (= (field (S4e i)) Second) (= (retval i) (rval (S4e i))) (soE (S3e i) (S4e i)))  ) ))

(assert (forall ((i I)) (=> (not (= (rval (S2e i)) zero)) (and (isBot (S3e i)) (isBot (S4e i)) ) ) ))

(assert (forall ((i I)) (=> (not (= (rval (S2e i)) zero)) (and (isR (S5e i)) (= (loc (S5e i)) (ls i)) (= (stype (S5e i)) S5t) (= (field (S5e i)) Second) (soE (S2e i) (S5e i)) ) ) ))
(assert (forall ((i I)) (=> (and (not (= (rval (S2e i)) zero)) (= (rval (S5e i)) zero)) (and (= (etype (S5e i)) U) (= (wval (S5e i)) (argval i) )
  ) ) ))
(assert (forall ((i I)) (=> (and (not (= (rval (S2e i)) zero)) (not (= (rval (S5e i)) zero))) (= (etype (S5e i)) R)  ) ))

(assert (forall ((i I)) (=> (not (= (rval (S2e i)) zero)) (and (isR (S6e i)) (= (loc (S6e i)) slot) (= (stype (S6e i)) S6t) (= (field (S6e i)) Default) (soE (S5e i) (S6e i)) ) ) ))
(assert (forall ((i I)) (=> (and (not (= (rval (S2e i)) zero)) (= (rval (S6e i)) (ls i))) (and (= (etype (S6e i)) U) (= (wval (S6e i)) (newloc i) )) ) ))
(assert (forall ((i I)) (=> (and (not (= (rval (S2e i)) zero)) (not (= (rval (S6e i)) (ls i)))) (= (etype (S6e i)) R)  ) ))

(assert (forall ((i I)) (=> (and (not (= (rval (S2e i)) zero)) (= (rval (S5e i)) zero)) (and (= (etype (S7e i)) R)  (= (loc (S7e i)) (ls i)) (= (stype (S7e i)) S7t) (= (field (S7e i)) First) (= (retval i) (rval (S7e i))) (soE (S6e i) (S7e i))) ) ))
(assert (forall ((i I)) (=> (and (not (= (rval (S2e i)) zero)) (not (= (rval (S5e i)) zero))) (and (= (stype (S7e i)) Bot) (= (retval i) BOTVAL))  ) ))

(assert (forall ((i1 I) (i2 I)) (=> (= (S1e i1) (S1e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (S2e i1) (S2e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (S3e i1) (S3e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (S4e i1) (S4e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (S5e i1) (S5e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (S6e i1) (S6e i2)) (= i1 i2) ) ))
(assert (forall ((i1 I) (i2 I)) (=> (= (S7e i1) (S7e i2)) (= i1 i2) ) ))

;Exchanger Specification
; (declare-fun match (I I) Bool)
; (assert (forall ((i1 I) (i2 I)) (= (match i1 i2) (and (= (argval i1) (retval i2)) (= (argval i2) (retval i1)) ) ) ))
; (assert (forall ((i1 I) (i2 I)) (=> (not (= i1 i2))(not (= (argval i1) (argval i2)))) ))

;AddRem
 (assert (not (= (retval in1) BOTVAL)))
 (assert  (not (= (retval in1) (argval in2))))
 (assert (not (= (retval in1) (argval in3))))
 (assert (not (= (retval in1) (argval in4))))

;Injective
 ; (assert (= (retval in1) (argval in3)))
 ; (assert (= (retval in2) (argval in3)))

;Exchange
; (assert (= (retval in1) (argval in2)))
; (assert (not (= (retval in2) (argval in1))))


;Consistency Policy
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (soE e1 e2) (vis e2 e3)) (vis e1 e3)  )) )
;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (vis e1 e2) (soE e2 e3)) (vis e1 e3)  )) )

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
