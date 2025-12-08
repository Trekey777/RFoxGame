(define (domain tufox)
  (:requirements :strips :typing :negative-preconditions)
  (:types agent room)
  (:predicates
    (connected ?a - room ?b - room)
    (at ?agent - agent ?room - room)
    (alive ?agent - agent)
    (inspected ?agent - agent)
    (not-inspected ?agent - agent)
    (is-detective ?agent - agent)
  )

  (:action move
    :parameters (?agent - agent ?from - room ?to - room)
    :precondition (and (alive ?agent) (at ?agent ?from) (connected ?from ?to))
    :effect (and (not (at ?agent ?from)) (at ?agent ?to))
  )

  (:action inspect
    :parameters (?det - agent ?target - agent ?room - room)
    :precondition (and (is-detective ?det) (alive ?det) (alive ?target) (at ?det ?room) (at ?target ?room) (not-inspected ?target))
    :effect (and (inspected ?target) (not (not-inspected ?target)))
  )
)