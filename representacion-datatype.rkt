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

<chip> := <chip p rim>
prim−chip ( chip−prim )
        
        := chip ( −−> {(port)}∗)
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


(define-datatype circuito circuito?
  (simple-circuit (in (list-of symbol?))
                  (out (list-of symbol?))
                  (chip chip?))
  (complex-circuit (circ circuito?)
                   (lcircs (list-of circuito?))
                   (input (list-of symbol?))
                   (output (list-of symbol?))))

(define-datatype chip chip?
  (prim-chip (chip-prim chip-prim?))
  (comp-chip (in (list-of symbol?))
             (out (list-of symbol?))
             (circ circuito?)))

(define-datatype chip-prim chip-prim?
  (chip-and)
  (chip-or)
  (chip-not)
  (chip-xor)
  (chip-nand)
  (chip-nor)
  (chip-xnor))

;; Ejemplo 2: circuito simple con un chip primitivo
(define ejemplo-2
  (simple-circuit
    '(x y z)
    '(out)
    (prim-chip (chip-and))
  )
)



;; Ejemplo 3: circuito complejo con dos circuitos simples
(define ejemplo-3
  (complex-circuit
    (simple-circuit
      '(a b)
      '(c)
      (prim-chip (chip-or))
    )
    (list
      (simple-circuit
        '(d e)
        '(f)
        (prim-chip (chip-xor))
      )
    )
    '(a b d e)
    '(c f)
  )
)



;; Ejemplo 4: circuito complejo con un chip complejo
(define ejemplo-4
  (complex-circuit
    (simple-circuit
      '(g h)
      '(i)
      (comp-chip
        '(IN1 IN2)
        '(OUT1)
        (simple-circuit
          '(j k)
          '(l)
          (prim-chip (chip-not))
        )
      )
    )
    (list)
    '(g h)
    '(i)
  )
)



;; Ejemplo 5: Circuitos complejos anidados
(define ejemplo-5
  (complex-circuit
    (complex-circuit
      (simple-circuit
        '(m n)
        '(o)
        (prim-chip (chip-and))
      )
      (list
        (simple-circuit
          '(p q)
          '(r)
          (prim-chip (chip-or))
        )
      )
      '(m n p q)
      '(o r)
    )
    (list)
    '(m n p q)
    '(o r)
  )
)

(display (circuito? ejemplo-2))
(newline)
(display (circuito? ejemplo-3))
(newline)
(display (circuito? ejemplo-4))
(newline)
(display (circuito? ejemplo-5))
(newline)
(display ejemplo-5)
(newline)
(display ejemplo-4)
(newline)
(display ejemplo-3)
(newline)
(display ejemplo-2)