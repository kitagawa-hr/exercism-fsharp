module SpaceAge

// TODO: define the Planet type

type Planet =
    | Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

let age (planet: Planet) (seconds: int64): float =
    match planet with
    | Earth -> double seconds / 31557600.0
    | Mercury -> double seconds / (31557600.0 * 0.2408467)
    | Venus -> double seconds / (31557600.0 * 0.61519726)
    | Mars -> double seconds / (31557600.0 * 1.8808158)
    | Jupiter -> double seconds / (31557600.0 * 11.862615)
    | Saturn -> double seconds / (31557600.0 * 29.447498)
    | Uranus -> double seconds / (31557600.0 * 84.016846)
    | Neptune -> double seconds / (31557600.0 * 164.79132)