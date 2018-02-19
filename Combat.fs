module Combat

type WeaponType =
    | Machinegun
    | Rockets
    | AntiAir

type ArmorType =
    | Minimal
    | Light
    | Medium
    | Hard

let damageAgainstArmor (armor: ArmorType) = function
    | Machinegun ->
        match armor with
        | Minimal -> 0.70
        | Light -> 0.40
        | Medium -> 0.10
        | Hard -> 0.0

    | Rockets ->    
        match armor with
        | Minimal -> 0.75
        | Light -> 0.65
        | Medium -> 0.50
        | Hard -> 0.40

    | AntiAir ->
        match armor with
        | Minimal -> 0.80
        | Light -> 0.70
        | Medium -> 0.15
        | Hard -> 0.05