(define (problem tufox-instance)
  (:domain tufox)
  (:objects
    detective player bunny1 bunny2 bunny3 bunny4 - agent
    kitchen living_room bathroom bedroom balcony - room
  )
  (:init
    (alive detective)
    (alive player)
    (at detective balcony)
    (at player kitchen)
    (connected kitchen living_room)
    (connected living_room kitchen)
    (connected living_room bathroom)
    (connected bathroom living_room)
    (connected living_room bedroom)
    (connected bedroom living_room)
    (connected bedroom balcony)
    (connected balcony bedroom)
    (fox player)
  )
  (:goal (inspected player))
)
