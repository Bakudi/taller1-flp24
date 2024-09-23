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
          [else ("Mensaje desconocido" mensaje)])))

(define (chip-and symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-and]
          [(eq? mensaje 'symbol) symbol]
          [else ("Mensaje desconocido" mensaje)])))

(define (chip-not symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-not]
          [(eq? mensaje 'symbol) symbol]
          [else ("Mensaje desconocido" mensaje)])))

(define (chip-xor symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-xor]
          [(eq? mensaje 'symbol) symbol]
          [else ("Mensaje desconocido" mensaje)])))

(define (chip-nand symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-nand]
          [(eq? mensaje 'symbol) symbol]
          [else ("Mensaje desconocido" mensaje)])))

(define (chip-nor symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-nor]
          [(eq? mensaje 'symbol) symbol]
          [else ("Mensaje desconocido" mensaje)])))

(define (chip-xnor symbol)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'chip-xnor]
          [(eq? mensaje 'symbol) symbol]
          [else ("Mensaje desconocido" mensaje)])))

;; constructores de chips

(define (prim-chip chip-prim symbol)
  (chip-prim symbol))

(define (comp-chip in out circ)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'comp-chip]
          [(eq? mensaje 'in) in]
          [(eq? mensaje 'out) out]
          [(eq? mensaje 'circ) circ]
          [else ("Mensaje desconocido" mensaje)])))

;; Constructor de circuito simple
(define (circ-simple in out chip)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'simple-circuit]
          [(eq? mensaje 'in) in]
          [(eq? mensaje 'out) out]
          [(eq? mensaje 'chip) chip]
          [else ("Mensaje desconocido" mensaje)])))

;; Constructor de circuito compuesto
(define (circ-comp circ lcircs in out)
  (lambda (mensaje)
    (cond [(eq? mensaje 'tipo) 'circ-comp]
          [(eq? mensaje 'circ) circ]
          [(eq? mensaje 'lcircs) lcircs]
          [(eq? mensaje 'in) in]
          [(eq? mensaje 'out) out]
          [else ("Mensaje desconocido" mensaje)])))

;; Observadores de chips primitivos
(define or-chip (prim-chip chip-or 'A))
(define not-chip (prim-chip chip-not 'B))
(define xor-chip (prim-chip chip-xor 'C))

;; Observadores de circuitos simples
(define circuito-simple1 (circ-simple '(cable1 cable2) '(cable3 cable4) or-chip))
(define circuito-simple2 (circ-simple '(cableA cableB) '(cableC cableD) not-chip))
(define circuito-simple3 (circ-simple '(cablex cabley) '(cablew cablez) xor-chip))

;; Observadores de chips compuestos
(define chip1 (comp-chip '(port1 port2) '(port3 port4) circuito-simple1))
(define chip2 (comp-chip '(portA portB) '(portC portD) circuito-simple2))
(define chip3 (comp-chip '(portX portY) '(portW portZ) circuito-simple3))

;; Observadores de circuitos complejos
(define circuito-complejo1 (circ-comp circuito-simple1 (list circuito-simple2) '(ABD CFG) '(DBA GFC)))
(define circuito-complejo2 (circ-comp circuito-simple3 (list circuito-simple2) '(XYZ JKL) '(ZYX LKJ)))