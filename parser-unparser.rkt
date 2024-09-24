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


;; Definición de expresiones en Datatype

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

;; parse
(define parse
  (lambda (expr)
    (cond
      [(circuito? expr)
       (cases circuito? expr
         [(simple-circuit in out chip)
          (list 'simple-circuit in out (parse chip))]
         [(complex-circuit circ lcircs input output)
          (list 'complex-circuit (parse circ) (map parse lcircs) input output)])]
      [(chip? expr)
       (cases chip? expr
         [(prim-chip chip-prim)
          (list 'prim-chip (parse-chip-prim chip-prim))]
         [(comp-chip in out circ)
          (list 'comp-chip in out (parse circ))])]
      [(chip-prim? expr)
       (cases chip-prim? expr
         [(chip-and) 'chip-and]
         [(chip-or) 'chip-or]
         [(chip-not) 'chip-not]
         [(chip-xor) 'chip-xor]
         [(chip-nand) 'chip-nand]
         [(chip-nor) 'chip-nor]
         [(chip-xnor) 'chip-xnor])])))

;; unparse
(define unparse
  (lambda (circuit)
    (cond
      [(circuito? circuit)
       (cases circuito? circuit
         [(simple-circuit in out chip)
          (simple-circuit in out (unparse chip))]
         [(complex-circuit circ lcircs input output)
          (complex-circuit (unparse circ) (map unparse lcircs) input output)])]
      [(chip? circuit)
       (cases chip? circuit
         [(prim-chip chip-prim)
          (prim-chip (unparse-chip-prim chip-prim))]
         [(comp-chip in out circ)
          (comp-chip in out (unparse circ))])]
      [(chip-prim? circuit)
       (cases chip-prim? circuit
         [(chip-and) (chip-and)]
         [(chip-or) (chip-or)]
         [(chip-not) (chip-not)]
         [(chip-xor) (chip-xor)]
         [(chip-nand) (chip-nand)]
         [(chip-nor) (chip-nor)]
         [(chip-xnor) (chip-xnor)])])))