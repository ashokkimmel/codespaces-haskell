module MyLib (Plugin) where
import GHC.Plugins 
plugin :: Plugin 
plugin = defaultPlugin 
--[LHsDecl GhcPs]
--[XRec GhcPs (HsDecl GhcPs)]
--[XRec (GhcPass 'Parsed) (HsDecl (GhcPass 'Parsed))]
--[ GenLocated (Anno (HsDecl (GhcPass 'Parsed))) (HsDecl (GhcPass 'Parsed))]
--[ GenLocated (Anno (HsDecl (GhcPass 'Parsed))) (HsDecl (GhcPass 'Parsed))]
--[((Anno (HsDecl (GhcPass 'Parsed))),(HsDecl (GhcPass 'Parsed)))]
--[(SrcSpanAnnA,(HsDecl (GhcPass 'Parsed)))]
--[(SrcAnn AnnListItem,(HsDecl (GhcPass 'Parsed)))]
--[(SrcSpanAnn' (EpAnn AnnListItem),(HsDecl (GhcPass 'Parsed)))]
-- some of the 
-- HsDecl (GhcPass 'Parsed)
-- would be 
-- ((XValD (GhcPass 'Parsed)),(HsBind (GhcPass 'Parsed)))
-- (NoExtField,(HsBind (GhcPass 'Parsed))) -- NoExtField is ()
-- the question is, what is 
-- (HsBind (GhcPass 'Parsed))
-- HsBindLR (GhcPass 'Parsed) (GhcPass 'Parsed)
