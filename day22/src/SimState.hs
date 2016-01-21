{-# LANGUAGE TemplateHaskell #-}
module SimState where

import Data.Function (on)
import Lens.Micro.TH

data EffectID = Shield | Poison | Recharge deriving (Eq, Ord, Show)

data SimState = Sim {
    _bossHP, _bossDmg,
    _playerHP, _playerMana, _playerArmor,
    _manaSpent :: !Int,
    _effects :: [(EffectID, Int)],
    _spellTrace :: [String]
    } deriving Show

instance Eq SimState where
    s1 == s2 = all (\eq -> eq s1 s2) [
        (==) `on` _bossHP,
        (==) `on` _playerHP,
        (==) `on` _playerMana,
        (==) `on` _effects]

instance Ord SimState where
    compare s1 s2 = foldMap (\cmp -> cmp s1 s2) [
        compare `on` _bossHP,
        compare `on` _playerHP,
        compare `on` _playerMana,
        compare `on` _effects]

makeLenses ''SimState
