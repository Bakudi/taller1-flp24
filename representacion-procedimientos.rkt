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

<chip> := <chip p rim>
prim−chip ( chip−prim )
        
        := chip ( −−> {(port)}∗)
                ( <−− {(port)}∗)
                <circuito>
comp−chip (in,out,circ)                    

<chip prim> := prim o r
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

;; constructores de chips primitivos

(define (chip-or symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-or]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

(define (chip-and symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-and]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

(define (chip-not symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-not]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

(define (chip-xor symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-xor]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

(define (chip-nand symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-nand]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

(define (chip-nor symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-nor]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

(define (chip-xnor symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-xnor]
          [(eq? mensaje 'symbol) symbol]
          [else (error "Mensaje desconocido" mensaje)])))

;; constructores de chips

(define (prim-chip chip-prim symbol)
  (chip-prim symbol))

(define (comp-chip in out circ)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'comp-chip]
          [(eq? mensaje 'in) in]
          [(eq? mensaje 'out) out]
          [(eq? mensaje 'circ) circ]
          [else (error "Mensaje desconocido" mensaje)])))