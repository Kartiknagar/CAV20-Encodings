(declare-sort E)
;NOTE: Below you can add more instances
(declare-datatypes () ((I (in1) (in2) (in3) (in4); (in5); (in6);(in7)(in8);(in9)(in10)
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
(define-fun sameloc ((e1 E) (e2 E)) Bool
(and (= (field e1) (field e2)) (= (loc e1) (loc e2)))
)
(define-fun sameses ((i1 I) (i2 I)) Bool
(= (sess i1) (sess i2))
)

(declare-fun initval (V FieldType) V)
(declare-fun rfinit (E) Bool)


;Local Variables
(declare-fun enqVar1 (I) V)
(declare-fun deqVar1 (I) V) ;h
(declare-fun deqVar2 (I) V) ;n
(declare-fun deqVar3 (I) V) ;result

;Locations
(declare-const enqlock V)
(declare-const deqlock V)
(declare-const tail V)
(declare-const head V)
(declare-const sentinelNode V)

;Values
(declare-const zero V)
(declare-const one V)
(declare-const EMPTY V)
(declare-const NULL V)


;Encoding of Last Writer Wins
;Ideally, the first commented constraint below is the ideal encoding, but it does not work well with Z3 since the constraint is not in EPR (there is an existential inside a universal quantifier). DO NOT UNCOMMENT IT. Instead, we use the equivalent second constraint below it.
;NOTE : For the second constraint, you must add more clauses depending upon the number of invocations (i.e. size of the sort I). For example, currently it is written for 4 invocations, with the clauses for the 5th invocation being commented out.

;(assert (forall ((e1 E)) (exists ((e2 E)) (=> (isR e1) (or (and (rfinit e1) (= (rval e1) (initval (loc e1) (field e1)))) (and (isM e2) (sameloc e1 e2) (vis e2 e1) (= (wval e2) (rval e1)) (rf e2 e1) )) ) ) ))
(assert (forall ((e E)) (=> (isR e) (or (rf (D1e in1) e) (rf (D2e in1) e) (rf (D3e in1) e) (rf (D4e in1) e) (rf (D5e in1) e) (rf (D6e in1) e) (rf (D7e in1) e) (rf (D1e in2) e) (rf (D2e in2) e) (rf (D3e in2) e) (rf (D4e in2) e) (rf (D5e in2) e) (rf (D6e in2) e) (rf (D7e in2) e)
(rf (D1e in3) e) (rf (D2e in3) e) (rf (D3e in3) e) (rf (D4e in3) e) (rf (D5e in3) e) (rf (D6e in3) e) (rf (D7e in3) e)
(rf (D1e in4) e) (rf (D2e in4) e) (rf (D3e in4) e) (rf (D4e in4) e) (rf (D5e in4) e) (rf (D6e in4) e) (rf (D7e in4) e)
;(rf (D1e in5) e) (rf (D2e in5) e) (rf (D3e in5) e) (rf (D4e in5) e) (rf (D5e in5) e) (rf (D6e in5) e) (rf (D7e in5) e)
  (rf (E1e in1) e) (rf (E2e in1) e) (rf (E3e in1) e) (rf (E4e in1) e) (rf (E5e in1) e) (rf (E6e in1) e) (rf (E7e in1) e)
    (rf (E1e in2) e) (rf (E2e in2) e) (rf (E3e in2) e) (rf (E4e in2) e) (rf (E5e in2) e) (rf (E6e in2) e) (rf (E7e in2) e)
      (rf (E1e in3) e) (rf (E2e in3) e) (rf (E3e in3) e) (rf (E4e in3) e) (rf (E5e in3) e) (rf (E6e in3) e) (rf (E7e in3) e)
      (rf (E1e in4) e) (rf (E2e in4) e) (rf (E3e in4) e) (rf (E4e in4) e) (rf (E5e in4) e) (rf (E6e in4) e) (rf (E7e in4) e)
;      (rf (E1e in5) e) (rf (E2e in5) e) (rf (E3e in5) e) (rf (E4e in5) e) (rf (E5e in5) e) (rf (E6e in5) e) (rf (E7e in5) e)
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

; Alternate way to encode Last Writer Wins. IGNORE for now.
; (declare-fun writes? (I V FieldType) Bool)
; (declare-fun writeevent (I V FieldType) E)
;
; (assert (forall ((e2 E)) (=> (and (isR e2) (not (isBot e2)) (=> (writes? in1 (loc e2) (field e2)) (not (rf (writeevent in1 (loc e2) (field e2)) e2)) )
; (=> (writes? in2 (loc e2) (field e2)) (not (rf (writeevent in2 (loc e2) (field e2)) e2)) )
; (=> (writes? in3 (loc e2) (field e2)) (not (rf (writeevent in3 (loc e2) (field e2)) e2)) )
; (=> (writes? in4 (loc e2) (field e2)) (not (rf (writeevent in4 (loc e2) (field e2)) e2)) )
; (=> (writes? in5 (loc e2) (field e2)) (not (rf (writeevent in5 (loc e2) (field e2)) e2)) )
; (=> (writes? in6 (loc e2) (field e2)) (not (rf (writeevent in6 (loc e2) (field e2)) e2)) )
; (=> (writes? in7 (loc e2) (field e2)) (not (rf (writeevent in7 (loc e2) (field e2)) e2)) )
; (=> (writes? in8 (loc e2) (field e2)) (not (rf (writeevent in8 (loc e2) (field e2)) e2)) )
; (=> (writes? in9 (loc e2) (field e2)) (not (rf (writeevent in9 (loc e2) (field e2)) e2)) )
; (=> (writes? in10 (loc e2) (field e2)) (not (rf (writeevent in10 (loc e2) (field e2)) e2)) )
;  ) (rfinit e2)) ))

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

(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i1) Enq) (= (itype i2) Enq) (soI i1 i2)) (soE (E7e i1) (E1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i1) Enq) (= (itype i2) Deq) (soI i1 i2)) (soE (E7e i1) (D1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i1) Deq) (= (deqVar2 i1) NULL) (= (itype i2) Enq) (soI i1 i2)) (soE (D4e i1) (E1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i1) Deq) (not (= (deqVar2 i1) NULL)) (= (itype i2) Enq) (soI i1 i2)) (soE (D7e i1) (E1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i1) Deq) (= (deqVar2 i1) NULL) (= (itype i2) Deq) (soI i1 i2)) (soE (D4e i1) (D1e i2)))))
(assert (forall ((i1 I) (i2 I)) (=> (and (= (itype i1) Deq) (not (= (deqVar2 i1) NULL)) (= (itype i2) Deq) (soI i1 i2)) (soE (D7e i1) (D1e i2)))))


(assert (not (= enqlock tail)))
(assert (not (= enqlock sentinelNode)))
(assert (not (= tail sentinelNode)))
(assert (not (= head sentinelNode)))
(assert (not (= tail head)))
(assert (not (= enqlock deqlock)))
(assert (not (= enqlock head)))
(assert (not (= head deqlock)))
(assert (not (= tail deqlock)))


(assert (not (= zero one)))
(assert (not (= NULL sentinelNode)))


(assert (= (initval enqlock Default) zero ))
(assert (= (initval deqlock Default) zero ))
(assert (= (initval tail Default) sentinelNode ))
(assert (= (initval head Default) sentinelNode ))
(assert (forall ((l V)) (= (initval l Next) NULL) ))
(assert (forall ((l V)) (= (initval l Val) zero) ))


(assert (forall((i I)) (not (= (argval i) EMPTY))))
(assert (forall((i I)) (not (= (newloc i) NULL))))
(assert (forall((i I)) (not (= (newloc i) sentinelNode))))
(assert (forall ((i1 I) (i2 I)) (=> (= (newloc i1) (newloc i2)) (= i1 i2) ) ))


;Encoding of Dequeue
(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D1e i)) zero) (= (stype (D1e i)) D1t) (= (loc (D1e i)) deqlock) (= (field (D1e i)) Default) (= (etype (D1e i)) U)
(= (wval (D1e i)) one)) ) ))

(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D1e i) (D2e i)) ) ) )
(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D2e i)) (deqVar1 i)) (= (stype (D2e i)) D2t) (= (loc (D2e i)) head) (= (field (D2e i)) Default) (= (etype (D2e i)) R)
) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq)  (soE (D2e i) (D3e i))) ) )
(assert (forall ((i I)) (=> (= (itype i) Deq) (and (= (rval (D3e i)) (deqVar2 i)) (= (stype (D3e i)) D3t) (= (loc (D3e i)) (deqVar1 i)) (= (field (D3e i)) Next) (= (etype (D3e i)) R)) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D3e i) (D4e i)) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Deq) (= (deqVar2 i) NULL)) (and (= (retval i) EMPTY) (= (stype (D4e i)) D4t) (= (stype (D5e i)) Bot) (= (stype (D6e i)) Bot) (= (stype (D7e i)) Bot) (= (wval (D4e i)) zero) (= (loc (D4e i)) deqlock) (= (field (D4e i)) Default) (= (etype (D4e i)) W))  ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D3e i) (D5e i)) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (deqVar2 i) NULL))) (and (= (stype (D4e i)) Bot) (= (etype (D5e i)) R) (= (stype (D5e i)) D5t) (= (loc (D5e i)) (deqVar2 i)) (= (field (D5e i)) Val) (= (rval (D5e i)) (deqVar3 i))) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D5e i) (D6e i)) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (deqVar2 i) NULL))) (and  (= (etype (D6e i)) W)  (= (loc (D6e i)) head) (= (stype (D6e i)) D6t) (= (field (D6e i)) Default) (= (wval (D6e i)) (deqVar2 i))) ) ))


(assert (forall ((i I)) (=> (= (itype i) Deq) (soE (D6e i) (D7e i)) ) ))
(assert (forall ((i I)) (=> (and (= (itype i) Deq) (not (= (deqVar2 i) NULL))) (and (= (retval i) (deqVar3 i)) (= (etype (D7e i)) W) (= (stype (D7e i)) D7t) (= (loc (D7e i)) deqlock) (= (field (D7e i)) Default) (= (wval (D7e i)) zero)) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D1e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D2e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D3e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D4e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D5e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D6e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Enq) (= (stype (D7e i)) Bot ) )))

;Encoding of Enqueue

(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E1e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E2e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E3e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E4e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E5e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E6e i)) Bot ) )))
(assert (forall ((i I)) (=> (= (itype i) Deq) (= (stype (E7e i)) Bot ) )))

(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E1e i)) U)  (= (loc (E1e i)) enqlock) (= (stype (E1e i)) E1t) (= (field (E1e i)) Default) (= (wval (E1e i)) one) (= (rval (E1e i)) zero)) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E1e i) (E2e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E2e i)) W)  (= (loc (E2e i)) (newloc i)) (= (stype (E2e i)) E2t) (= (field (E2e i)) Val) (= (wval (E2e i)) (argval i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E2e i) (E3e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E3e i)) W)  (= (loc (E3e i)) (newloc i)) (= (stype (E3e i)) E3t) (= (field (E3e i)) Next) (= (wval (E3e i)) NULL) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E3e i) (E4e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E4e i)) R)  (= (loc (E4e i)) tail) (= (stype (E4e i)) E4t) (= (field (E4e i)) Default) (= (rval (E4e i)) (enqVar1 i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E4e i) (E5e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E5e i)) W)  (= (loc (E5e i)) (enqVar1 i)) (= (stype (E5e i)) E5t) (= (field (E5e i)) Next) (= (wval (E5e i)) (newloc i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E5e i) (E6e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E6e i)) W)  (= (loc (E6e i)) tail) (= (stype (E6e i)) E6t) (= (field (E6e i)) Default) (= (wval (E6e i)) (newloc i)) ) ) ))


(assert (forall ((i I)) (=> (= (itype i) Enq) (soE (E6e i) (E7e i)) ) ))
(assert (forall ((i I)) (=> (= (itype i) Enq) (and (= (etype (E7e i)) W)  (= (loc (E7e i)) enqlock) (= (stype (E7e i)) E7t) (= (field (E7e i)) Default) (= (wval (E7e i)) zero) ) ) ))


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
;NOTE: Uncomment any particular specification as needed
(declare-fun matchm (I I) Bool)
(assert (forall ((i1 I) (i2 I)) (= (matchm i1 i2) (and (= (itype i1) Enq) (= (itype i2) Deq) (= (argval i1) (retval i2)))) ))
(assert (forall ((i1 I) (i2 I)) (=> (not (= i1 i2))(not (= (argval i1) (argval i2)))) ))

;AddRem
  ; (assert (= (itype in1) Deq))
  ; (assert (not (= (retval in1) EMPTY)))
  ; (assert (forall ((i I)) (not (and (= (itype i) Enq) (= (argval i) (retval in1)))) ) )
  ;

;Injective
; (assert (and (= (itype in1) Enq) (= (itype in2) Deq) (= (argval in1) (retval in2))))
; (assert (and (= (itype in1) Enq) (= (itype in3) Deq) (= (argval in1) (retval in3))))

;NOTE: bef is the hb relation among invocations used in the paper.
(declare-fun bef (I I) Bool)
(assert (forall ((i1 I) (i2 I)) (=>  (bef i1 i2)  (not (bef i2 i1))) ))
(assert (forall ((e1 I) (e2 I) (e3 I)) (=> (and (bef e1 e2) (bef e2 e3))  (bef e1 e3)) ))
(assert (forall ((i1 I) (i2 I)) (=>  (matchm i1 i2)  (bef i1 i2))) )
(assert (forall ((i1 I) (i2 I)) (=>  (soI i1 i2)  (bef i1 i2))) )

;NOTE: Again, Ideally the first commented constraint below is the ideal encoding of the hb relation. Unfortunately it is again not in EPR and so Z3 does not perform well with it. DO NOT UNCOMMENT IT. Instead, the alternate encoding is below it. Note that this alternate encoding is dependent on the number of invocations. Currently it is written for 4 invocations, but more constraints (similar to the current ones) need to be added as the number of invocations increase.
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

;NOTE: In all the specifications below, you can use soI instead of bef to obtain alternate specifications with session order instead of happens-before
;Empty
; (declare-const inv1 I)
;  (declare-const inv2 I)
;  (assert (and (= (itype inv1) Deq) (= (retval inv1) EMPTY)))
;  (assert (and (= (itype inv2) Enq) (bef inv2 inv1)))
;  (assert (forall ((i I)) (not (matchm inv2 i)) ))

;FIFO-1
 ;
 ;  (declare-const inv1 I)
 ; (declare-const inv2 I)
 ;  (declare-const inv3 I)
 ;  (assert (and (= (itype inv1) Enq) (matchm inv2 inv3) (bef inv1 inv2) ))
 ;  (assert (forall ((i I))  (not (matchm inv1 i))  ))

 ;FIFO-2
 (declare-const inv1 I)
 (declare-const inv2 I)
 (declare-const inv3 I)
 (declare-const inv4 I)
  (assert (and (matchm inv1 inv4) (matchm inv2 inv3) (bef inv1 inv2) (bef inv3 inv4) ))

;Consistency Policy
;NOTE: Comment or Uncomment consistency policies as required
 (assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (soE e1 e2) (vis e2 e3)) (vis e1 e3)  )) ) ;MW
 (assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (vis e1 e2) (soE e2 e3)) (vis e1 e3)  )) ) ;MR
;(assert (forall ((e1 E) (e2 E)) (=>  (soE e1 e2) (vis e1 e2)  )) ) ;RMW
(assert (forall ((e1 E) (e2 E) (e3 E) (e4 E)) (=> (and (vis e1 e2) (soE e2 e3) (vis e3 e4)) (vis e1 e4)  )) ) ;WFR


;(assert (forall ((e1 E) (e2 E) (e3 E)) (=> (and (tot e1 e2) (vis e2 e3)) (vis e1 e3)  )) ) ;CV
;(assert (forall ((e1 E) (e2 E)) (=>  (tot e1 e2) (vis e1 e2)  )) ) ;CC


;Below, I had experimented with different per-method consistency policies. IGNORE for now.
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
