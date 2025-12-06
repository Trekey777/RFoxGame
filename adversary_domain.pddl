(define (domain tufox)
  (:requirements :strips)
  (:predicates
    (connected ?a ?b)
    (at ?agent ?room)
    (alive ?agent)
    (inspected ?agent)
    (fox ?agent)
  )

  (:action move
    :parameters (?agent ?from ?to)
    :precondition (and (alive ?agent) (at ?agent ?from) (connected ?from ?to))
    :effect (and (not (at ?agent ?from)) (at ?agent ?to))
  )

  (:action inspect
    :parameters (?det ?target ?room)
    :precondition (and (alive ?det) (alive ?target) (at ?det ?room) (at ?target ?room) (not (inspected ?target)))
    :effect (inspected ?target)
  )
)
