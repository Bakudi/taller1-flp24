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
(define (chip-tipo chip)
  (chip 'tipo))

(define (chip-symbol chip)
  (chip 'symbol))

;; Observadores de circuitos simples
(define (circ-simple-in circ)
  (circ 'in))

(define (circ-simple-out circ)
  (circ 'out))

(define (circ-simple-chip circ)
  (circ 'chip))


;; Función para imprimir cualquier tipo de circuito
(define (imprimir-circuito circ)
  (cond [(eq? (circ 'tipo) 'simple-circuit)
         (imprimir-circuito-simple circ)]
        [(eq? (circ 'tipo) 'circ-comp)
         (imprimir-circuito-comp circ)]))

;; Observadores de chips compuestos
(define (comp-chip-in chip)
  (chip 'in))

(define (comp-chip-out chip)
  (chip 'out))

(define (comp-chip-circ chip)
  (chip 'circ))

;; Observadores de circuitos complejos
(define (circ-comp-circ circ)
  (circ 'circ))

(define (circ-comp-lcircs circ)
  (circ 'lcircs))

(define (circ-comp-in circ)
  (circ 'in))

(define (circ-comp-out circ)
  (circ 'out))

;; Función para imprimir un chip primitivo
(define (imprimir-chip chip)
  (display "Chip tipo: ")
  (display (chip-tipo chip))
  (newline)
  (display "Símbolo: ")
  (display (chip-symbol chip))
  (newline))

  ;; Función para imprimir un chip compuesto
(define (imprimir-comp-chip chip)
  (display "Chip compuesto - Entradas: ")
  (display (comp-chip-in chip))
  (newline)
  (display "Salidas: ")
  (display (comp-chip-out chip))
  (newline)
  (display "Circuito: ")
  (imprimir-circuito (comp-chip-circ chip))
  (newline))

  ;; Función para imprimir un circuito simple
(define (imprimir-circuito-simple circ)
  (display "Circuito simple - Entradas: ")
  (display (circ-simple-in circ))
  (newline)
  (display "Salidas: ")
  (display (circ-simple-out circ))
  (newline)
  (display "Chip: ")
  (imprimir-chip (circ-simple-chip circ))
  (newline))

  ;; Función para imprimir un circuito compuesto
(define (imprimir-circuito-comp circ)
  (display "Circuito compuesto - Entradas: ")
  (display (circ-comp-in circ))
  (newline)
  (display "Salidas: ")
  (display (circ-comp-out circ))
  (newline)
  (display "Circuito principal: ")
  (imprimir-circuito (circ-comp-circ circ))
  (newline)
  (display "Circuitos secundarios: ")
  (for-each imprimir-circuito (circ-comp-lcircs circ))
  (newline))


;; Observadores de chips primitivos
(define or-chip (prim-chip chip-or 'A))
(define not-chip (prim-chip chip-not 'B))
(define xor-chip (prim-chip chip-xor 'C))

;; Observadores de circuitos simples
(define circuito-simple1 (circ-simple '(cable1 cable2) '(cable3 cable4) or-chip))
(define circuito-simple2 (circ-simple '(cableA cableB) '(cableC cableD) not-chip))
(define circuito-simple3 (circ-simple '(cablex cabley) '(cablew cablez) xor-chip))

;; Observadores de chips
(define chip1 (comp-chip '(port1 port2) '(port3 port4) circuito-simple1))
(define chip2 (comp-chip '(portA portB) '(portC portD) circuito-simple2))
(define chip3 (comp-chip '(portX portY) '(portW portZ) circuito-simple3))

;; Observadores de circuitos complejos
(define circuito-complejo1 (circ-comp circuito-simple1 (list circuito-simple2) '(ABD CFG) '(DBA GFC)))
(define circuito-complejo2 (circ-comp circuito-simple3 (list circuito-simple2) '(XYZ JKL) '(ZYX LKJ)))



(imprimir-comp-chip chip1)
(imprimir-comp-chip chip2)
(imprimir-comp-chip chip3)
(imprimir-circuito circuito-simple1)
(imprimir-circuito circuito-simple2)
(imprimir-circuito circuito-complejo1)
(imprimir-circuito circuito-complejo2)