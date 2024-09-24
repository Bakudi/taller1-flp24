;;Autores: Juan Camilo Gutierrez Viveros, 2159874. Andres Felipe Rojas, 2160328.
#lang eopl

#| gramatica

<circuito> := circ_simple ({cable}*)
                          ({cable}*)
                          <chip>
simple-circuit(in out chip)

            := circ_comp <circuito> {<circuito>}+
                         input {cable} *
                         output {cable} *
                complex-circuit (circ lcircs in out)

<chip> 
        := <chip-prim>
        prim−chip ( chip−prim )
        
        := comp-chip ( −−> {(port)}∗)
                ( <−− {(port)}∗)
                <circuito>
comp−chip (in,out,circ)                    

<chip prim> := prim or
            chip−or ( )
    := prim and
    chip−and ( )

    := prim not
    chip−not ( )

    := prim xor
    chip−xor ( )

    := prim nand
    chip−xor ( )

    := prim nor
    chip−nor ( )

    := prim xnor
    chip−xnor ( )
|#

;;constructores de chips primarios

(define (chip-or)
    (lambda (symbol)
  (list 'chip-or symbol)))

(define (chip-and)
    (lambda (symbol)
  (list 'chip-and symbol)))

(define (chip-not)
    (lambda (symbol)
  (list 'chip-not symbol)))

(define (chip-xor)
    (lambda (symbol)
  (list 'chip-xor symbol)))

(define (chip-nand)
    (lambda (symbol)
  (list 'chip-nand symbol)))

(define (chip-nor)
    (lambda (symbol)
  (list 'chip-nor symbol)))

(define (chip-xnor)
    (lambda (symbol)
  (list 'chip-xnor symbol)))

;; constructores de chip 
(define (prim-chip)
  (lambda (chip-prim symbol)
    ((chip-prim) symbol)
  ))

(define (comp-chip)
    (lambda (in out circ)
        (list 'comp-chip in out circ)
    ))
;; constructores de circuitos
(define (circ_simple)
    (lambda (in out chip)
    (list 'simple-circuit in out chip))
    )

(define (circ-comp)
  (lambda (circ lcircs in out)
    (list 'circ-comp circ lcircs in out)))

;;observadores
;; observadores de chips primitivos
(define or-chip ((prim-chip) chip-or 'A))
(define not-chip ((prim-chip) chip-not 'B))
(define xor-chip ((prim-chip) chip-xor 'C))

;; observadores de circuitos simples
(define circuito-simple1 ((circ_simple) '(cable1 cable2) '(cable3 cable4) or-chip))
(define circuito-simple2 ((circ_simple) '(cableA cableB) '(cableC cableD) not-chip))
(define circuito-simple3 ((circ_simple) '(cablex cabley) '(cablew cablez) xor-chip))

;; observadores de chips
(define chip1 ((comp-chip) '(port1 port2) '(port3 port4) circuito-simple1))
(define chip2 ((comp-chip) '(portA portB) '(portC portD) circuito-simple2))
(define chip3 ((comp-chip) '(portX portY) '(portW portZ) circuito-simple3))

;;observadores de circuitos complejos

(define circuito-complejo1 ((circ-comp) circuito-simple1 (list circuito-simple2) '(ABD CFG) '(DBA GFC)))
(define circuito-complejo2 ((circ-comp) circuito-simple3 (list circuito-simple2) '(XYZ JKL) '(ZYX LKJ)))