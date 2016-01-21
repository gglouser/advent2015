module SimSearch (
    roundPart1,
    roundPart2,
    simObjective,
    score,
    mkSimState,
    module SimState
) where

import Control.Monad
import Control.Monad.State
import Lens.Micro
import Lens.Micro.Mtl
import SimState

type SimM = StateT SimState []

type SpellID = String
type Spell = (SpellID, Int, SimM ())

allSpells :: [Spell]
allSpells = [
    ("MagicMissile", 53, bossHP -= 4),
    ("Drain",        73, bossHP -= 2 >> playerHP += 2),
    ("Shield",      113, addEffect Shield 6 >> playerArmor += 7),
    ("Poison",      173, addEffect Poison 6),
    ("Recharge",    229, addEffect Recharge 5)]

cast :: Spell -> SimM ()
cast (sp, mana, action) = do
    playerMana -= mana
    guard . (>= 0) =<< use playerMana
    action
    manaSpent += mana
    spellTrace %= (++ [sp])

addEffect :: EffectID -> Int -> SimM ()
addEffect eff t = do
    effs <- use effects
    guard . notElem eff $ map fst effs
    effects .= (eff, t) : effs

tickAllEffects :: SimM ()
tickAllEffects = do
    effs <- use effects
    mapM_ (uncurry tickEffect) effs
    effects .= [(e,t-1) | (e,t) <- effs, t > 1]

tickEffect :: EffectID -> Int -> SimM ()
tickEffect Shield   1 = playerArmor -= 7
tickEffect Shield   _ = return ()
tickEffect Poison   _ = bossHP -= 3
tickEffect Recharge _ = playerMana += 101

damagePlayer :: Int -> SimM ()
damagePlayer d = do
    playerHP -= d
    guard . (> 0) =<< use playerHP

bossAttack :: SimM ()
bossAttack = damagePlayer . max 1 =<< (-) <$> use bossDmg <*> use playerArmor

unlessBossDead :: SimM () -> SimM ()
unlessBossDead m = flip unless m . (<= 0) =<< use bossHP

combatRound :: [Spell] -> SimM ()
combatRound spells = do
    tickAllEffects
    unlessBossDead $ do
        cast =<< lift spells
        tickAllEffects
        unlessBossDead bossAttack

hardMode :: SimM ()
hardMode = damagePlayer 1

roundPart1 :: SimState -> [SimState]
roundPart1 = execStateT $ combatRound allSpells

roundPart2 :: SimState -> [SimState]
roundPart2 = execStateT $ hardMode >> combatRound allSpells

-- Estimate minimum mana-to-kill.
-- 1. must spend at least 53 mana per turn
-- 2. max damage per turn is 4 (MM) + 2*3 (Poison) = 10
--    est. turns-to-kill is ceiling(bossHP / 10)
estMTK :: SimState -> Int
estMTK s | s^.bossHP <= 0 = 0
         | otherwise      = 53 * ((s^.bossHP + 9) `div` 10)

score :: SimState -> Int
score s = s^.manaSpent + estMTK s

simObjective :: SimState -> Maybe Int
simObjective s = s^.manaSpent <$ guard (s^.bossHP <= 0)

mkSimState :: Int -> Int -> SimState
mkSimState initBossHP initBossDmg = Sim {
    _bossHP = initBossHP, _bossDmg = initBossDmg,
    _playerHP = 50, _playerMana = 500, _playerArmor = 0,
    _effects = [],
    _manaSpent = 0, _spellTrace = []
    }
