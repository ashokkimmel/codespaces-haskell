class AsError unittype generalizederror where 
    asError :: unittype -> generalizederror
    fromError :: generalizederror -> Maybe unittype
    default asError :: GAsError unnittype (Rep generalizederror) => unittype -> generalizederror
    