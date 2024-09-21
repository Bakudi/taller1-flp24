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

;;constructores

(define-type cable symbol)
(define-type port symbol)

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

(define (prim-chip)
  (lambda (chip-prim symbol)
    ((chip-prim) symbol)
  ))

(define (comp-chip)
    (lambda (in out circ)
        (list 'comp-chip in out circ)
    ))

(define circ_simple
    (lambda ()
    
    
    ))
