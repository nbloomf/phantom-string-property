{-# LANGUAGE DataKinds, KindSignatures, BangPatterns #-}

{-|
Module      : Data.String.Validate.Length
Description : Length predicates
Copyright   : (c) 2018 Automattic, Inc.
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
Portability : POSIX

Type-level constraints on the length of a string.
-}
module Data.String.Validate.Length (
  -- * Exact Length
    LengthIs(..)

  -- ** Convenience Types

  -- | Generated using

  -- | @seq 128 | awk '{printf "\n-- | Satisfies @length x == %d@.\nlengthIs%d = LengthIs (Proxy :: Proxy %d) :: LengthIs %d\n", $1, $1, $1, $1}'@

  -- | @seq 128 | awk '{printf "  , lengthIs%d\n", $1}'@

  , lengthIs1
  , lengthIs2
  , lengthIs3
  , lengthIs4
  , lengthIs5
  , lengthIs6
  , lengthIs7
  , lengthIs8
  , lengthIs9
  , lengthIs10
  , lengthIs11
  , lengthIs12
  , lengthIs13
  , lengthIs14
  , lengthIs15
  , lengthIs16
  , lengthIs17
  , lengthIs18
  , lengthIs19
  , lengthIs20
  , lengthIs21
  , lengthIs22
  , lengthIs23
  , lengthIs24
  , lengthIs25
  , lengthIs26
  , lengthIs27
  , lengthIs28
  , lengthIs29
  , lengthIs30
  , lengthIs31
  , lengthIs32
  , lengthIs33
  , lengthIs34
  , lengthIs35
  , lengthIs36
  , lengthIs37
  , lengthIs38
  , lengthIs39
  , lengthIs40
  , lengthIs41
  , lengthIs42
  , lengthIs43
  , lengthIs44
  , lengthIs45
  , lengthIs46
  , lengthIs47
  , lengthIs48
  , lengthIs49
  , lengthIs50
  , lengthIs51
  , lengthIs52
  , lengthIs53
  , lengthIs54
  , lengthIs55
  , lengthIs56
  , lengthIs57
  , lengthIs58
  , lengthIs59
  , lengthIs60
  , lengthIs61
  , lengthIs62
  , lengthIs63
  , lengthIs64
  , lengthIs65
  , lengthIs66
  , lengthIs67
  , lengthIs68
  , lengthIs69
  , lengthIs70
  , lengthIs71
  , lengthIs72
  , lengthIs73
  , lengthIs74
  , lengthIs75
  , lengthIs76
  , lengthIs77
  , lengthIs78
  , lengthIs79
  , lengthIs80
  , lengthIs81
  , lengthIs82
  , lengthIs83
  , lengthIs84
  , lengthIs85
  , lengthIs86
  , lengthIs87
  , lengthIs88
  , lengthIs89
  , lengthIs90
  , lengthIs91
  , lengthIs92
  , lengthIs93
  , lengthIs94
  , lengthIs95
  , lengthIs96
  , lengthIs97
  , lengthIs98
  , lengthIs99
  , lengthIs100
  , lengthIs101
  , lengthIs102
  , lengthIs103
  , lengthIs104
  , lengthIs105
  , lengthIs106
  , lengthIs107
  , lengthIs108
  , lengthIs109
  , lengthIs110
  , lengthIs111
  , lengthIs112
  , lengthIs113
  , lengthIs114
  , lengthIs115
  , lengthIs116
  , lengthIs117
  , lengthIs118
  , lengthIs119
  , lengthIs120
  , lengthIs121
  , lengthIs122
  , lengthIs123
  , lengthIs124
  , lengthIs125
  , lengthIs126
  , lengthIs127
  , lengthIs128

  -- * At Least
  , LengthAtLeast(..)

  -- ** Convenience Types

  -- | Generated using

  -- | seq 128 | awk '{printf "\n-- | Satisfies @length x >= %d@.\nlengthAtLeast%d = LengthAtLeast (Proxy :: Proxy %d) :: LengthAtLeast %d\n", $1, $1, $1, $1}'

  -- | @seq 128 | awk '{printf "  , lengthAtLeast%d\n", $1}'@

  , lengthAtLeast1
  , lengthAtLeast2
  , lengthAtLeast3
  , lengthAtLeast4
  , lengthAtLeast5
  , lengthAtLeast6
  , lengthAtLeast7
  , lengthAtLeast8
  , lengthAtLeast9
  , lengthAtLeast10
  , lengthAtLeast11
  , lengthAtLeast12
  , lengthAtLeast13
  , lengthAtLeast14
  , lengthAtLeast15
  , lengthAtLeast16
  , lengthAtLeast17
  , lengthAtLeast18
  , lengthAtLeast19
  , lengthAtLeast20
  , lengthAtLeast21
  , lengthAtLeast22
  , lengthAtLeast23
  , lengthAtLeast24
  , lengthAtLeast25
  , lengthAtLeast26
  , lengthAtLeast27
  , lengthAtLeast28
  , lengthAtLeast29
  , lengthAtLeast30
  , lengthAtLeast31
  , lengthAtLeast32
  , lengthAtLeast33
  , lengthAtLeast34
  , lengthAtLeast35
  , lengthAtLeast36
  , lengthAtLeast37
  , lengthAtLeast38
  , lengthAtLeast39
  , lengthAtLeast40
  , lengthAtLeast41
  , lengthAtLeast42
  , lengthAtLeast43
  , lengthAtLeast44
  , lengthAtLeast45
  , lengthAtLeast46
  , lengthAtLeast47
  , lengthAtLeast48
  , lengthAtLeast49
  , lengthAtLeast50
  , lengthAtLeast51
  , lengthAtLeast52
  , lengthAtLeast53
  , lengthAtLeast54
  , lengthAtLeast55
  , lengthAtLeast56
  , lengthAtLeast57
  , lengthAtLeast58
  , lengthAtLeast59
  , lengthAtLeast60
  , lengthAtLeast61
  , lengthAtLeast62
  , lengthAtLeast63
  , lengthAtLeast64
  , lengthAtLeast65
  , lengthAtLeast66
  , lengthAtLeast67
  , lengthAtLeast68
  , lengthAtLeast69
  , lengthAtLeast70
  , lengthAtLeast71
  , lengthAtLeast72
  , lengthAtLeast73
  , lengthAtLeast74
  , lengthAtLeast75
  , lengthAtLeast76
  , lengthAtLeast77
  , lengthAtLeast78
  , lengthAtLeast79
  , lengthAtLeast80
  , lengthAtLeast81
  , lengthAtLeast82
  , lengthAtLeast83
  , lengthAtLeast84
  , lengthAtLeast85
  , lengthAtLeast86
  , lengthAtLeast87
  , lengthAtLeast88
  , lengthAtLeast89
  , lengthAtLeast90
  , lengthAtLeast91
  , lengthAtLeast92
  , lengthAtLeast93
  , lengthAtLeast94
  , lengthAtLeast95
  , lengthAtLeast96
  , lengthAtLeast97
  , lengthAtLeast98
  , lengthAtLeast99
  , lengthAtLeast100
  , lengthAtLeast101
  , lengthAtLeast102
  , lengthAtLeast103
  , lengthAtLeast104
  , lengthAtLeast105
  , lengthAtLeast106
  , lengthAtLeast107
  , lengthAtLeast108
  , lengthAtLeast109
  , lengthAtLeast110
  , lengthAtLeast111
  , lengthAtLeast112
  , lengthAtLeast113
  , lengthAtLeast114
  , lengthAtLeast115
  , lengthAtLeast116
  , lengthAtLeast117
  , lengthAtLeast118
  , lengthAtLeast119
  , lengthAtLeast120
  , lengthAtLeast121
  , lengthAtLeast122
  , lengthAtLeast123
  , lengthAtLeast124
  , lengthAtLeast125
  , lengthAtLeast126
  , lengthAtLeast127
  , lengthAtLeast128

  -- * At Most
  , LengthAtMost(..)

  -- ** Convenience Types

  -- | Generated using

  -- | @seq 128 | awk '{printf "\n-- | Satisfies @length x <= %d@.\nlengthAtMost%d = LengthAtMost (Proxy :: Proxy %d) :: LengthAtMost %d\n", $1, $1, $1, $1}'@

  -- | @seq 128 | awk '{printf "  , lengthAtMost%d\n", $1}'@

  , lengthAtMost1
  , lengthAtMost2
  , lengthAtMost3
  , lengthAtMost4
  , lengthAtMost5
  , lengthAtMost6
  , lengthAtMost7
  , lengthAtMost8
  , lengthAtMost9
  , lengthAtMost10
  , lengthAtMost11
  , lengthAtMost12
  , lengthAtMost13
  , lengthAtMost14
  , lengthAtMost15
  , lengthAtMost16
  , lengthAtMost17
  , lengthAtMost18
  , lengthAtMost19
  , lengthAtMost20
  , lengthAtMost21
  , lengthAtMost22
  , lengthAtMost23
  , lengthAtMost24
  , lengthAtMost25
  , lengthAtMost26
  , lengthAtMost27
  , lengthAtMost28
  , lengthAtMost29
  , lengthAtMost30
  , lengthAtMost31
  , lengthAtMost32
  , lengthAtMost33
  , lengthAtMost34
  , lengthAtMost35
  , lengthAtMost36
  , lengthAtMost37
  , lengthAtMost38
  , lengthAtMost39
  , lengthAtMost40
  , lengthAtMost41
  , lengthAtMost42
  , lengthAtMost43
  , lengthAtMost44
  , lengthAtMost45
  , lengthAtMost46
  , lengthAtMost47
  , lengthAtMost48
  , lengthAtMost49
  , lengthAtMost50
  , lengthAtMost51
  , lengthAtMost52
  , lengthAtMost53
  , lengthAtMost54
  , lengthAtMost55
  , lengthAtMost56
  , lengthAtMost57
  , lengthAtMost58
  , lengthAtMost59
  , lengthAtMost60
  , lengthAtMost61
  , lengthAtMost62
  , lengthAtMost63
  , lengthAtMost64
  , lengthAtMost65
  , lengthAtMost66
  , lengthAtMost67
  , lengthAtMost68
  , lengthAtMost69
  , lengthAtMost70
  , lengthAtMost71
  , lengthAtMost72
  , lengthAtMost73
  , lengthAtMost74
  , lengthAtMost75
  , lengthAtMost76
  , lengthAtMost77
  , lengthAtMost78
  , lengthAtMost79
  , lengthAtMost80
  , lengthAtMost81
  , lengthAtMost82
  , lengthAtMost83
  , lengthAtMost84
  , lengthAtMost85
  , lengthAtMost86
  , lengthAtMost87
  , lengthAtMost88
  , lengthAtMost89
  , lengthAtMost90
  , lengthAtMost91
  , lengthAtMost92
  , lengthAtMost93
  , lengthAtMost94
  , lengthAtMost95
  , lengthAtMost96
  , lengthAtMost97
  , lengthAtMost98
  , lengthAtMost99
  , lengthAtMost100
  , lengthAtMost101
  , lengthAtMost102
  , lengthAtMost103
  , lengthAtMost104
  , lengthAtMost105
  , lengthAtMost106
  , lengthAtMost107
  , lengthAtMost108
  , lengthAtMost109
  , lengthAtMost110
  , lengthAtMost111
  , lengthAtMost112
  , lengthAtMost113
  , lengthAtMost114
  , lengthAtMost115
  , lengthAtMost116
  , lengthAtMost117
  , lengthAtMost118
  , lengthAtMost119
  , lengthAtMost120
  , lengthAtMost121
  , lengthAtMost122
  , lengthAtMost123
  , lengthAtMost124
  , lengthAtMost125
  , lengthAtMost126
  , lengthAtMost127
  , lengthAtMost128
) where

import Data.List (genericLength)
import Data.Proxy
import Data.Typeable
import GHC.TypeLits

import Data.String.Validate.Class



-- | Satisfies @length x == n@.
data LengthIs (n :: Nat) = LengthIs (Proxy n)
  deriving (Eq, Show, Typeable)

instance (KnownNat n) => StringProperty (LengthIs n) where
  validator (LengthIs !proxy) str =
    let k = natVal $! proxy in
    let m = genericLength str in
    if m == k
      then Right ()
      else
        let
          err = "Length is " ++ show m ++ ", but expected " ++ show k
        in
          Left [validationError err []]



-- | Satisfies @length x == 1@.
lengthIs1 = LengthIs (Proxy :: Proxy 1) :: LengthIs 1

-- | Satisfies @length x == 2@.
lengthIs2 = LengthIs (Proxy :: Proxy 2) :: LengthIs 2

-- | Satisfies @length x == 3@.
lengthIs3 = LengthIs (Proxy :: Proxy 3) :: LengthIs 3

-- | Satisfies @length x == 4@.
lengthIs4 = LengthIs (Proxy :: Proxy 4) :: LengthIs 4

-- | Satisfies @length x == 5@.
lengthIs5 = LengthIs (Proxy :: Proxy 5) :: LengthIs 5

-- | Satisfies @length x == 6@.
lengthIs6 = LengthIs (Proxy :: Proxy 6) :: LengthIs 6

-- | Satisfies @length x == 7@.
lengthIs7 = LengthIs (Proxy :: Proxy 7) :: LengthIs 7

-- | Satisfies @length x == 8@.
lengthIs8 = LengthIs (Proxy :: Proxy 8) :: LengthIs 8

-- | Satisfies @length x == 9@.
lengthIs9 = LengthIs (Proxy :: Proxy 9) :: LengthIs 9

-- | Satisfies @length x == 10@.
lengthIs10 = LengthIs (Proxy :: Proxy 10) :: LengthIs 10

-- | Satisfies @length x == 11@.
lengthIs11 = LengthIs (Proxy :: Proxy 11) :: LengthIs 11

-- | Satisfies @length x == 12@.
lengthIs12 = LengthIs (Proxy :: Proxy 12) :: LengthIs 12

-- | Satisfies @length x == 13@.
lengthIs13 = LengthIs (Proxy :: Proxy 13) :: LengthIs 13

-- | Satisfies @length x == 14@.
lengthIs14 = LengthIs (Proxy :: Proxy 14) :: LengthIs 14

-- | Satisfies @length x == 15@.
lengthIs15 = LengthIs (Proxy :: Proxy 15) :: LengthIs 15

-- | Satisfies @length x == 16@.
lengthIs16 = LengthIs (Proxy :: Proxy 16) :: LengthIs 16

-- | Satisfies @length x == 17@.
lengthIs17 = LengthIs (Proxy :: Proxy 17) :: LengthIs 17

-- | Satisfies @length x == 18@.
lengthIs18 = LengthIs (Proxy :: Proxy 18) :: LengthIs 18

-- | Satisfies @length x == 19@.
lengthIs19 = LengthIs (Proxy :: Proxy 19) :: LengthIs 19

-- | Satisfies @length x == 20@.
lengthIs20 = LengthIs (Proxy :: Proxy 20) :: LengthIs 20

-- | Satisfies @length x == 21@.
lengthIs21 = LengthIs (Proxy :: Proxy 21) :: LengthIs 21

-- | Satisfies @length x == 22@.
lengthIs22 = LengthIs (Proxy :: Proxy 22) :: LengthIs 22

-- | Satisfies @length x == 23@.
lengthIs23 = LengthIs (Proxy :: Proxy 23) :: LengthIs 23

-- | Satisfies @length x == 24@.
lengthIs24 = LengthIs (Proxy :: Proxy 24) :: LengthIs 24

-- | Satisfies @length x == 25@.
lengthIs25 = LengthIs (Proxy :: Proxy 25) :: LengthIs 25

-- | Satisfies @length x == 26@.
lengthIs26 = LengthIs (Proxy :: Proxy 26) :: LengthIs 26

-- | Satisfies @length x == 27@.
lengthIs27 = LengthIs (Proxy :: Proxy 27) :: LengthIs 27

-- | Satisfies @length x == 28@.
lengthIs28 = LengthIs (Proxy :: Proxy 28) :: LengthIs 28

-- | Satisfies @length x == 29@.
lengthIs29 = LengthIs (Proxy :: Proxy 29) :: LengthIs 29

-- | Satisfies @length x == 30@.
lengthIs30 = LengthIs (Proxy :: Proxy 30) :: LengthIs 30

-- | Satisfies @length x == 31@.
lengthIs31 = LengthIs (Proxy :: Proxy 31) :: LengthIs 31

-- | Satisfies @length x == 32@.
lengthIs32 = LengthIs (Proxy :: Proxy 32) :: LengthIs 32

-- | Satisfies @length x == 33@.
lengthIs33 = LengthIs (Proxy :: Proxy 33) :: LengthIs 33

-- | Satisfies @length x == 34@.
lengthIs34 = LengthIs (Proxy :: Proxy 34) :: LengthIs 34

-- | Satisfies @length x == 35@.
lengthIs35 = LengthIs (Proxy :: Proxy 35) :: LengthIs 35

-- | Satisfies @length x == 36@.
lengthIs36 = LengthIs (Proxy :: Proxy 36) :: LengthIs 36

-- | Satisfies @length x == 37@.
lengthIs37 = LengthIs (Proxy :: Proxy 37) :: LengthIs 37

-- | Satisfies @length x == 38@.
lengthIs38 = LengthIs (Proxy :: Proxy 38) :: LengthIs 38

-- | Satisfies @length x == 39@.
lengthIs39 = LengthIs (Proxy :: Proxy 39) :: LengthIs 39

-- | Satisfies @length x == 40@.
lengthIs40 = LengthIs (Proxy :: Proxy 40) :: LengthIs 40

-- | Satisfies @length x == 41@.
lengthIs41 = LengthIs (Proxy :: Proxy 41) :: LengthIs 41

-- | Satisfies @length x == 42@.
lengthIs42 = LengthIs (Proxy :: Proxy 42) :: LengthIs 42

-- | Satisfies @length x == 43@.
lengthIs43 = LengthIs (Proxy :: Proxy 43) :: LengthIs 43

-- | Satisfies @length x == 44@.
lengthIs44 = LengthIs (Proxy :: Proxy 44) :: LengthIs 44

-- | Satisfies @length x == 45@.
lengthIs45 = LengthIs (Proxy :: Proxy 45) :: LengthIs 45

-- | Satisfies @length x == 46@.
lengthIs46 = LengthIs (Proxy :: Proxy 46) :: LengthIs 46

-- | Satisfies @length x == 47@.
lengthIs47 = LengthIs (Proxy :: Proxy 47) :: LengthIs 47

-- | Satisfies @length x == 48@.
lengthIs48 = LengthIs (Proxy :: Proxy 48) :: LengthIs 48

-- | Satisfies @length x == 49@.
lengthIs49 = LengthIs (Proxy :: Proxy 49) :: LengthIs 49

-- | Satisfies @length x == 50@.
lengthIs50 = LengthIs (Proxy :: Proxy 50) :: LengthIs 50

-- | Satisfies @length x == 51@.
lengthIs51 = LengthIs (Proxy :: Proxy 51) :: LengthIs 51

-- | Satisfies @length x == 52@.
lengthIs52 = LengthIs (Proxy :: Proxy 52) :: LengthIs 52

-- | Satisfies @length x == 53@.
lengthIs53 = LengthIs (Proxy :: Proxy 53) :: LengthIs 53

-- | Satisfies @length x == 54@.
lengthIs54 = LengthIs (Proxy :: Proxy 54) :: LengthIs 54

-- | Satisfies @length x == 55@.
lengthIs55 = LengthIs (Proxy :: Proxy 55) :: LengthIs 55

-- | Satisfies @length x == 56@.
lengthIs56 = LengthIs (Proxy :: Proxy 56) :: LengthIs 56

-- | Satisfies @length x == 57@.
lengthIs57 = LengthIs (Proxy :: Proxy 57) :: LengthIs 57

-- | Satisfies @length x == 58@.
lengthIs58 = LengthIs (Proxy :: Proxy 58) :: LengthIs 58

-- | Satisfies @length x == 59@.
lengthIs59 = LengthIs (Proxy :: Proxy 59) :: LengthIs 59

-- | Satisfies @length x == 60@.
lengthIs60 = LengthIs (Proxy :: Proxy 60) :: LengthIs 60

-- | Satisfies @length x == 61@.
lengthIs61 = LengthIs (Proxy :: Proxy 61) :: LengthIs 61

-- | Satisfies @length x == 62@.
lengthIs62 = LengthIs (Proxy :: Proxy 62) :: LengthIs 62

-- | Satisfies @length x == 63@.
lengthIs63 = LengthIs (Proxy :: Proxy 63) :: LengthIs 63

-- | Satisfies @length x == 64@.
lengthIs64 = LengthIs (Proxy :: Proxy 64) :: LengthIs 64

-- | Satisfies @length x == 65@.
lengthIs65 = LengthIs (Proxy :: Proxy 65) :: LengthIs 65

-- | Satisfies @length x == 66@.
lengthIs66 = LengthIs (Proxy :: Proxy 66) :: LengthIs 66

-- | Satisfies @length x == 67@.
lengthIs67 = LengthIs (Proxy :: Proxy 67) :: LengthIs 67

-- | Satisfies @length x == 68@.
lengthIs68 = LengthIs (Proxy :: Proxy 68) :: LengthIs 68

-- | Satisfies @length x == 69@.
lengthIs69 = LengthIs (Proxy :: Proxy 69) :: LengthIs 69

-- | Satisfies @length x == 70@.
lengthIs70 = LengthIs (Proxy :: Proxy 70) :: LengthIs 70

-- | Satisfies @length x == 71@.
lengthIs71 = LengthIs (Proxy :: Proxy 71) :: LengthIs 71

-- | Satisfies @length x == 72@.
lengthIs72 = LengthIs (Proxy :: Proxy 72) :: LengthIs 72

-- | Satisfies @length x == 73@.
lengthIs73 = LengthIs (Proxy :: Proxy 73) :: LengthIs 73

-- | Satisfies @length x == 74@.
lengthIs74 = LengthIs (Proxy :: Proxy 74) :: LengthIs 74

-- | Satisfies @length x == 75@.
lengthIs75 = LengthIs (Proxy :: Proxy 75) :: LengthIs 75

-- | Satisfies @length x == 76@.
lengthIs76 = LengthIs (Proxy :: Proxy 76) :: LengthIs 76

-- | Satisfies @length x == 77@.
lengthIs77 = LengthIs (Proxy :: Proxy 77) :: LengthIs 77

-- | Satisfies @length x == 78@.
lengthIs78 = LengthIs (Proxy :: Proxy 78) :: LengthIs 78

-- | Satisfies @length x == 79@.
lengthIs79 = LengthIs (Proxy :: Proxy 79) :: LengthIs 79

-- | Satisfies @length x == 80@.
lengthIs80 = LengthIs (Proxy :: Proxy 80) :: LengthIs 80

-- | Satisfies @length x == 81@.
lengthIs81 = LengthIs (Proxy :: Proxy 81) :: LengthIs 81

-- | Satisfies @length x == 82@.
lengthIs82 = LengthIs (Proxy :: Proxy 82) :: LengthIs 82

-- | Satisfies @length x == 83@.
lengthIs83 = LengthIs (Proxy :: Proxy 83) :: LengthIs 83

-- | Satisfies @length x == 84@.
lengthIs84 = LengthIs (Proxy :: Proxy 84) :: LengthIs 84

-- | Satisfies @length x == 85@.
lengthIs85 = LengthIs (Proxy :: Proxy 85) :: LengthIs 85

-- | Satisfies @length x == 86@.
lengthIs86 = LengthIs (Proxy :: Proxy 86) :: LengthIs 86

-- | Satisfies @length x == 87@.
lengthIs87 = LengthIs (Proxy :: Proxy 87) :: LengthIs 87

-- | Satisfies @length x == 88@.
lengthIs88 = LengthIs (Proxy :: Proxy 88) :: LengthIs 88

-- | Satisfies @length x == 89@.
lengthIs89 = LengthIs (Proxy :: Proxy 89) :: LengthIs 89

-- | Satisfies @length x == 90@.
lengthIs90 = LengthIs (Proxy :: Proxy 90) :: LengthIs 90

-- | Satisfies @length x == 91@.
lengthIs91 = LengthIs (Proxy :: Proxy 91) :: LengthIs 91

-- | Satisfies @length x == 92@.
lengthIs92 = LengthIs (Proxy :: Proxy 92) :: LengthIs 92

-- | Satisfies @length x == 93@.
lengthIs93 = LengthIs (Proxy :: Proxy 93) :: LengthIs 93

-- | Satisfies @length x == 94@.
lengthIs94 = LengthIs (Proxy :: Proxy 94) :: LengthIs 94

-- | Satisfies @length x == 95@.
lengthIs95 = LengthIs (Proxy :: Proxy 95) :: LengthIs 95

-- | Satisfies @length x == 96@.
lengthIs96 = LengthIs (Proxy :: Proxy 96) :: LengthIs 96

-- | Satisfies @length x == 97@.
lengthIs97 = LengthIs (Proxy :: Proxy 97) :: LengthIs 97

-- | Satisfies @length x == 98@.
lengthIs98 = LengthIs (Proxy :: Proxy 98) :: LengthIs 98

-- | Satisfies @length x == 99@.
lengthIs99 = LengthIs (Proxy :: Proxy 99) :: LengthIs 99

-- | Satisfies @length x == 100@.
lengthIs100 = LengthIs (Proxy :: Proxy 100) :: LengthIs 100

-- | Satisfies @length x == 101@.
lengthIs101 = LengthIs (Proxy :: Proxy 101) :: LengthIs 101

-- | Satisfies @length x == 102@.
lengthIs102 = LengthIs (Proxy :: Proxy 102) :: LengthIs 102

-- | Satisfies @length x == 103@.
lengthIs103 = LengthIs (Proxy :: Proxy 103) :: LengthIs 103

-- | Satisfies @length x == 104@.
lengthIs104 = LengthIs (Proxy :: Proxy 104) :: LengthIs 104

-- | Satisfies @length x == 105@.
lengthIs105 = LengthIs (Proxy :: Proxy 105) :: LengthIs 105

-- | Satisfies @length x == 106@.
lengthIs106 = LengthIs (Proxy :: Proxy 106) :: LengthIs 106

-- | Satisfies @length x == 107@.
lengthIs107 = LengthIs (Proxy :: Proxy 107) :: LengthIs 107

-- | Satisfies @length x == 108@.
lengthIs108 = LengthIs (Proxy :: Proxy 108) :: LengthIs 108

-- | Satisfies @length x == 109@.
lengthIs109 = LengthIs (Proxy :: Proxy 109) :: LengthIs 109

-- | Satisfies @length x == 110@.
lengthIs110 = LengthIs (Proxy :: Proxy 110) :: LengthIs 110

-- | Satisfies @length x == 111@.
lengthIs111 = LengthIs (Proxy :: Proxy 111) :: LengthIs 111

-- | Satisfies @length x == 112@.
lengthIs112 = LengthIs (Proxy :: Proxy 112) :: LengthIs 112

-- | Satisfies @length x == 113@.
lengthIs113 = LengthIs (Proxy :: Proxy 113) :: LengthIs 113

-- | Satisfies @length x == 114@.
lengthIs114 = LengthIs (Proxy :: Proxy 114) :: LengthIs 114

-- | Satisfies @length x == 115@.
lengthIs115 = LengthIs (Proxy :: Proxy 115) :: LengthIs 115

-- | Satisfies @length x == 116@.
lengthIs116 = LengthIs (Proxy :: Proxy 116) :: LengthIs 116

-- | Satisfies @length x == 117@.
lengthIs117 = LengthIs (Proxy :: Proxy 117) :: LengthIs 117

-- | Satisfies @length x == 118@.
lengthIs118 = LengthIs (Proxy :: Proxy 118) :: LengthIs 118

-- | Satisfies @length x == 119@.
lengthIs119 = LengthIs (Proxy :: Proxy 119) :: LengthIs 119

-- | Satisfies @length x == 120@.
lengthIs120 = LengthIs (Proxy :: Proxy 120) :: LengthIs 120

-- | Satisfies @length x == 121@.
lengthIs121 = LengthIs (Proxy :: Proxy 121) :: LengthIs 121

-- | Satisfies @length x == 122@.
lengthIs122 = LengthIs (Proxy :: Proxy 122) :: LengthIs 122

-- | Satisfies @length x == 123@.
lengthIs123 = LengthIs (Proxy :: Proxy 123) :: LengthIs 123

-- | Satisfies @length x == 124@.
lengthIs124 = LengthIs (Proxy :: Proxy 124) :: LengthIs 124

-- | Satisfies @length x == 125@.
lengthIs125 = LengthIs (Proxy :: Proxy 125) :: LengthIs 125

-- | Satisfies @length x == 126@.
lengthIs126 = LengthIs (Proxy :: Proxy 126) :: LengthIs 126

-- | Satisfies @length x == 127@.
lengthIs127 = LengthIs (Proxy :: Proxy 127) :: LengthIs 127

-- | Satisfies @length x == 128@.
lengthIs128 = LengthIs (Proxy :: Proxy 128) :: LengthIs 128



-- | Satisfies @length x >= n@.
data LengthAtLeast (n :: Nat) = LengthAtLeast (Proxy n)
  deriving (Eq, Show, Typeable)

instance (KnownNat n) => StringProperty (LengthAtLeast n) where
  validator (LengthAtLeast !proxy) str =
    let k = natVal $! proxy in
    let m = genericLength str in
    if m >= k
      then Right ()
      else
        let
          err = "Length is " ++ show m ++ ", but expected at least " ++ show k
        in
          Left [validationError err []]



-- | Satisfies @length x >= 1@.
lengthAtLeast1 = LengthAtLeast (Proxy :: Proxy 1) :: LengthAtLeast 1

-- | Satisfies @length x >= 2@.
lengthAtLeast2 = LengthAtLeast (Proxy :: Proxy 2) :: LengthAtLeast 2

-- | Satisfies @length x >= 3@.
lengthAtLeast3 = LengthAtLeast (Proxy :: Proxy 3) :: LengthAtLeast 3

-- | Satisfies @length x >= 4@.
lengthAtLeast4 = LengthAtLeast (Proxy :: Proxy 4) :: LengthAtLeast 4

-- | Satisfies @length x >= 5@.
lengthAtLeast5 = LengthAtLeast (Proxy :: Proxy 5) :: LengthAtLeast 5

-- | Satisfies @length x >= 6@.
lengthAtLeast6 = LengthAtLeast (Proxy :: Proxy 6) :: LengthAtLeast 6

-- | Satisfies @length x >= 7@.
lengthAtLeast7 = LengthAtLeast (Proxy :: Proxy 7) :: LengthAtLeast 7

-- | Satisfies @length x >= 8@.
lengthAtLeast8 = LengthAtLeast (Proxy :: Proxy 8) :: LengthAtLeast 8

-- | Satisfies @length x >= 9@.
lengthAtLeast9 = LengthAtLeast (Proxy :: Proxy 9) :: LengthAtLeast 9

-- | Satisfies @length x >= 10@.
lengthAtLeast10 = LengthAtLeast (Proxy :: Proxy 10) :: LengthAtLeast 10

-- | Satisfies @length x >= 11@.
lengthAtLeast11 = LengthAtLeast (Proxy :: Proxy 11) :: LengthAtLeast 11

-- | Satisfies @length x >= 12@.
lengthAtLeast12 = LengthAtLeast (Proxy :: Proxy 12) :: LengthAtLeast 12

-- | Satisfies @length x >= 13@.
lengthAtLeast13 = LengthAtLeast (Proxy :: Proxy 13) :: LengthAtLeast 13

-- | Satisfies @length x >= 14@.
lengthAtLeast14 = LengthAtLeast (Proxy :: Proxy 14) :: LengthAtLeast 14

-- | Satisfies @length x >= 15@.
lengthAtLeast15 = LengthAtLeast (Proxy :: Proxy 15) :: LengthAtLeast 15

-- | Satisfies @length x >= 16@.
lengthAtLeast16 = LengthAtLeast (Proxy :: Proxy 16) :: LengthAtLeast 16

-- | Satisfies @length x >= 17@.
lengthAtLeast17 = LengthAtLeast (Proxy :: Proxy 17) :: LengthAtLeast 17

-- | Satisfies @length x >= 18@.
lengthAtLeast18 = LengthAtLeast (Proxy :: Proxy 18) :: LengthAtLeast 18

-- | Satisfies @length x >= 19@.
lengthAtLeast19 = LengthAtLeast (Proxy :: Proxy 19) :: LengthAtLeast 19

-- | Satisfies @length x >= 20@.
lengthAtLeast20 = LengthAtLeast (Proxy :: Proxy 20) :: LengthAtLeast 20

-- | Satisfies @length x >= 21@.
lengthAtLeast21 = LengthAtLeast (Proxy :: Proxy 21) :: LengthAtLeast 21

-- | Satisfies @length x >= 22@.
lengthAtLeast22 = LengthAtLeast (Proxy :: Proxy 22) :: LengthAtLeast 22

-- | Satisfies @length x >= 23@.
lengthAtLeast23 = LengthAtLeast (Proxy :: Proxy 23) :: LengthAtLeast 23

-- | Satisfies @length x >= 24@.
lengthAtLeast24 = LengthAtLeast (Proxy :: Proxy 24) :: LengthAtLeast 24

-- | Satisfies @length x >= 25@.
lengthAtLeast25 = LengthAtLeast (Proxy :: Proxy 25) :: LengthAtLeast 25

-- | Satisfies @length x >= 26@.
lengthAtLeast26 = LengthAtLeast (Proxy :: Proxy 26) :: LengthAtLeast 26

-- | Satisfies @length x >= 27@.
lengthAtLeast27 = LengthAtLeast (Proxy :: Proxy 27) :: LengthAtLeast 27

-- | Satisfies @length x >= 28@.
lengthAtLeast28 = LengthAtLeast (Proxy :: Proxy 28) :: LengthAtLeast 28

-- | Satisfies @length x >= 29@.
lengthAtLeast29 = LengthAtLeast (Proxy :: Proxy 29) :: LengthAtLeast 29

-- | Satisfies @length x >= 30@.
lengthAtLeast30 = LengthAtLeast (Proxy :: Proxy 30) :: LengthAtLeast 30

-- | Satisfies @length x >= 31@.
lengthAtLeast31 = LengthAtLeast (Proxy :: Proxy 31) :: LengthAtLeast 31

-- | Satisfies @length x >= 32@.
lengthAtLeast32 = LengthAtLeast (Proxy :: Proxy 32) :: LengthAtLeast 32

-- | Satisfies @length x >= 33@.
lengthAtLeast33 = LengthAtLeast (Proxy :: Proxy 33) :: LengthAtLeast 33

-- | Satisfies @length x >= 34@.
lengthAtLeast34 = LengthAtLeast (Proxy :: Proxy 34) :: LengthAtLeast 34

-- | Satisfies @length x >= 35@.
lengthAtLeast35 = LengthAtLeast (Proxy :: Proxy 35) :: LengthAtLeast 35

-- | Satisfies @length x >= 36@.
lengthAtLeast36 = LengthAtLeast (Proxy :: Proxy 36) :: LengthAtLeast 36

-- | Satisfies @length x >= 37@.
lengthAtLeast37 = LengthAtLeast (Proxy :: Proxy 37) :: LengthAtLeast 37

-- | Satisfies @length x >= 38@.
lengthAtLeast38 = LengthAtLeast (Proxy :: Proxy 38) :: LengthAtLeast 38

-- | Satisfies @length x >= 39@.
lengthAtLeast39 = LengthAtLeast (Proxy :: Proxy 39) :: LengthAtLeast 39

-- | Satisfies @length x >= 40@.
lengthAtLeast40 = LengthAtLeast (Proxy :: Proxy 40) :: LengthAtLeast 40

-- | Satisfies @length x >= 41@.
lengthAtLeast41 = LengthAtLeast (Proxy :: Proxy 41) :: LengthAtLeast 41

-- | Satisfies @length x >= 42@.
lengthAtLeast42 = LengthAtLeast (Proxy :: Proxy 42) :: LengthAtLeast 42

-- | Satisfies @length x >= 43@.
lengthAtLeast43 = LengthAtLeast (Proxy :: Proxy 43) :: LengthAtLeast 43

-- | Satisfies @length x >= 44@.
lengthAtLeast44 = LengthAtLeast (Proxy :: Proxy 44) :: LengthAtLeast 44

-- | Satisfies @length x >= 45@.
lengthAtLeast45 = LengthAtLeast (Proxy :: Proxy 45) :: LengthAtLeast 45

-- | Satisfies @length x >= 46@.
lengthAtLeast46 = LengthAtLeast (Proxy :: Proxy 46) :: LengthAtLeast 46

-- | Satisfies @length x >= 47@.
lengthAtLeast47 = LengthAtLeast (Proxy :: Proxy 47) :: LengthAtLeast 47

-- | Satisfies @length x >= 48@.
lengthAtLeast48 = LengthAtLeast (Proxy :: Proxy 48) :: LengthAtLeast 48

-- | Satisfies @length x >= 49@.
lengthAtLeast49 = LengthAtLeast (Proxy :: Proxy 49) :: LengthAtLeast 49

-- | Satisfies @length x >= 50@.
lengthAtLeast50 = LengthAtLeast (Proxy :: Proxy 50) :: LengthAtLeast 50

-- | Satisfies @length x >= 51@.
lengthAtLeast51 = LengthAtLeast (Proxy :: Proxy 51) :: LengthAtLeast 51

-- | Satisfies @length x >= 52@.
lengthAtLeast52 = LengthAtLeast (Proxy :: Proxy 52) :: LengthAtLeast 52

-- | Satisfies @length x >= 53@.
lengthAtLeast53 = LengthAtLeast (Proxy :: Proxy 53) :: LengthAtLeast 53

-- | Satisfies @length x >= 54@.
lengthAtLeast54 = LengthAtLeast (Proxy :: Proxy 54) :: LengthAtLeast 54

-- | Satisfies @length x >= 55@.
lengthAtLeast55 = LengthAtLeast (Proxy :: Proxy 55) :: LengthAtLeast 55

-- | Satisfies @length x >= 56@.
lengthAtLeast56 = LengthAtLeast (Proxy :: Proxy 56) :: LengthAtLeast 56

-- | Satisfies @length x >= 57@.
lengthAtLeast57 = LengthAtLeast (Proxy :: Proxy 57) :: LengthAtLeast 57

-- | Satisfies @length x >= 58@.
lengthAtLeast58 = LengthAtLeast (Proxy :: Proxy 58) :: LengthAtLeast 58

-- | Satisfies @length x >= 59@.
lengthAtLeast59 = LengthAtLeast (Proxy :: Proxy 59) :: LengthAtLeast 59

-- | Satisfies @length x >= 60@.
lengthAtLeast60 = LengthAtLeast (Proxy :: Proxy 60) :: LengthAtLeast 60

-- | Satisfies @length x >= 61@.
lengthAtLeast61 = LengthAtLeast (Proxy :: Proxy 61) :: LengthAtLeast 61

-- | Satisfies @length x >= 62@.
lengthAtLeast62 = LengthAtLeast (Proxy :: Proxy 62) :: LengthAtLeast 62

-- | Satisfies @length x >= 63@.
lengthAtLeast63 = LengthAtLeast (Proxy :: Proxy 63) :: LengthAtLeast 63

-- | Satisfies @length x >= 64@.
lengthAtLeast64 = LengthAtLeast (Proxy :: Proxy 64) :: LengthAtLeast 64

-- | Satisfies @length x >= 65@.
lengthAtLeast65 = LengthAtLeast (Proxy :: Proxy 65) :: LengthAtLeast 65

-- | Satisfies @length x >= 66@.
lengthAtLeast66 = LengthAtLeast (Proxy :: Proxy 66) :: LengthAtLeast 66

-- | Satisfies @length x >= 67@.
lengthAtLeast67 = LengthAtLeast (Proxy :: Proxy 67) :: LengthAtLeast 67

-- | Satisfies @length x >= 68@.
lengthAtLeast68 = LengthAtLeast (Proxy :: Proxy 68) :: LengthAtLeast 68

-- | Satisfies @length x >= 69@.
lengthAtLeast69 = LengthAtLeast (Proxy :: Proxy 69) :: LengthAtLeast 69

-- | Satisfies @length x >= 70@.
lengthAtLeast70 = LengthAtLeast (Proxy :: Proxy 70) :: LengthAtLeast 70

-- | Satisfies @length x >= 71@.
lengthAtLeast71 = LengthAtLeast (Proxy :: Proxy 71) :: LengthAtLeast 71

-- | Satisfies @length x >= 72@.
lengthAtLeast72 = LengthAtLeast (Proxy :: Proxy 72) :: LengthAtLeast 72

-- | Satisfies @length x >= 73@.
lengthAtLeast73 = LengthAtLeast (Proxy :: Proxy 73) :: LengthAtLeast 73

-- | Satisfies @length x >= 74@.
lengthAtLeast74 = LengthAtLeast (Proxy :: Proxy 74) :: LengthAtLeast 74

-- | Satisfies @length x >= 75@.
lengthAtLeast75 = LengthAtLeast (Proxy :: Proxy 75) :: LengthAtLeast 75

-- | Satisfies @length x >= 76@.
lengthAtLeast76 = LengthAtLeast (Proxy :: Proxy 76) :: LengthAtLeast 76

-- | Satisfies @length x >= 77@.
lengthAtLeast77 = LengthAtLeast (Proxy :: Proxy 77) :: LengthAtLeast 77

-- | Satisfies @length x >= 78@.
lengthAtLeast78 = LengthAtLeast (Proxy :: Proxy 78) :: LengthAtLeast 78

-- | Satisfies @length x >= 79@.
lengthAtLeast79 = LengthAtLeast (Proxy :: Proxy 79) :: LengthAtLeast 79

-- | Satisfies @length x >= 80@.
lengthAtLeast80 = LengthAtLeast (Proxy :: Proxy 80) :: LengthAtLeast 80

-- | Satisfies @length x >= 81@.
lengthAtLeast81 = LengthAtLeast (Proxy :: Proxy 81) :: LengthAtLeast 81

-- | Satisfies @length x >= 82@.
lengthAtLeast82 = LengthAtLeast (Proxy :: Proxy 82) :: LengthAtLeast 82

-- | Satisfies @length x >= 83@.
lengthAtLeast83 = LengthAtLeast (Proxy :: Proxy 83) :: LengthAtLeast 83

-- | Satisfies @length x >= 84@.
lengthAtLeast84 = LengthAtLeast (Proxy :: Proxy 84) :: LengthAtLeast 84

-- | Satisfies @length x >= 85@.
lengthAtLeast85 = LengthAtLeast (Proxy :: Proxy 85) :: LengthAtLeast 85

-- | Satisfies @length x >= 86@.
lengthAtLeast86 = LengthAtLeast (Proxy :: Proxy 86) :: LengthAtLeast 86

-- | Satisfies @length x >= 87@.
lengthAtLeast87 = LengthAtLeast (Proxy :: Proxy 87) :: LengthAtLeast 87

-- | Satisfies @length x >= 88@.
lengthAtLeast88 = LengthAtLeast (Proxy :: Proxy 88) :: LengthAtLeast 88

-- | Satisfies @length x >= 89@.
lengthAtLeast89 = LengthAtLeast (Proxy :: Proxy 89) :: LengthAtLeast 89

-- | Satisfies @length x >= 90@.
lengthAtLeast90 = LengthAtLeast (Proxy :: Proxy 90) :: LengthAtLeast 90

-- | Satisfies @length x >= 91@.
lengthAtLeast91 = LengthAtLeast (Proxy :: Proxy 91) :: LengthAtLeast 91

-- | Satisfies @length x >= 92@.
lengthAtLeast92 = LengthAtLeast (Proxy :: Proxy 92) :: LengthAtLeast 92

-- | Satisfies @length x >= 93@.
lengthAtLeast93 = LengthAtLeast (Proxy :: Proxy 93) :: LengthAtLeast 93

-- | Satisfies @length x >= 94@.
lengthAtLeast94 = LengthAtLeast (Proxy :: Proxy 94) :: LengthAtLeast 94

-- | Satisfies @length x >= 95@.
lengthAtLeast95 = LengthAtLeast (Proxy :: Proxy 95) :: LengthAtLeast 95

-- | Satisfies @length x >= 96@.
lengthAtLeast96 = LengthAtLeast (Proxy :: Proxy 96) :: LengthAtLeast 96

-- | Satisfies @length x >= 97@.
lengthAtLeast97 = LengthAtLeast (Proxy :: Proxy 97) :: LengthAtLeast 97

-- | Satisfies @length x >= 98@.
lengthAtLeast98 = LengthAtLeast (Proxy :: Proxy 98) :: LengthAtLeast 98

-- | Satisfies @length x >= 99@.
lengthAtLeast99 = LengthAtLeast (Proxy :: Proxy 99) :: LengthAtLeast 99

-- | Satisfies @length x >= 100@.
lengthAtLeast100 = LengthAtLeast (Proxy :: Proxy 100) :: LengthAtLeast 100

-- | Satisfies @length x >= 101@.
lengthAtLeast101 = LengthAtLeast (Proxy :: Proxy 101) :: LengthAtLeast 101

-- | Satisfies @length x >= 102@.
lengthAtLeast102 = LengthAtLeast (Proxy :: Proxy 102) :: LengthAtLeast 102

-- | Satisfies @length x >= 103@.
lengthAtLeast103 = LengthAtLeast (Proxy :: Proxy 103) :: LengthAtLeast 103

-- | Satisfies @length x >= 104@.
lengthAtLeast104 = LengthAtLeast (Proxy :: Proxy 104) :: LengthAtLeast 104

-- | Satisfies @length x >= 105@.
lengthAtLeast105 = LengthAtLeast (Proxy :: Proxy 105) :: LengthAtLeast 105

-- | Satisfies @length x >= 106@.
lengthAtLeast106 = LengthAtLeast (Proxy :: Proxy 106) :: LengthAtLeast 106

-- | Satisfies @length x >= 107@.
lengthAtLeast107 = LengthAtLeast (Proxy :: Proxy 107) :: LengthAtLeast 107

-- | Satisfies @length x >= 108@.
lengthAtLeast108 = LengthAtLeast (Proxy :: Proxy 108) :: LengthAtLeast 108

-- | Satisfies @length x >= 109@.
lengthAtLeast109 = LengthAtLeast (Proxy :: Proxy 109) :: LengthAtLeast 109

-- | Satisfies @length x >= 110@.
lengthAtLeast110 = LengthAtLeast (Proxy :: Proxy 110) :: LengthAtLeast 110

-- | Satisfies @length x >= 111@.
lengthAtLeast111 = LengthAtLeast (Proxy :: Proxy 111) :: LengthAtLeast 111

-- | Satisfies @length x >= 112@.
lengthAtLeast112 = LengthAtLeast (Proxy :: Proxy 112) :: LengthAtLeast 112

-- | Satisfies @length x >= 113@.
lengthAtLeast113 = LengthAtLeast (Proxy :: Proxy 113) :: LengthAtLeast 113

-- | Satisfies @length x >= 114@.
lengthAtLeast114 = LengthAtLeast (Proxy :: Proxy 114) :: LengthAtLeast 114

-- | Satisfies @length x >= 115@.
lengthAtLeast115 = LengthAtLeast (Proxy :: Proxy 115) :: LengthAtLeast 115

-- | Satisfies @length x >= 116@.
lengthAtLeast116 = LengthAtLeast (Proxy :: Proxy 116) :: LengthAtLeast 116

-- | Satisfies @length x >= 117@.
lengthAtLeast117 = LengthAtLeast (Proxy :: Proxy 117) :: LengthAtLeast 117

-- | Satisfies @length x >= 118@.
lengthAtLeast118 = LengthAtLeast (Proxy :: Proxy 118) :: LengthAtLeast 118

-- | Satisfies @length x >= 119@.
lengthAtLeast119 = LengthAtLeast (Proxy :: Proxy 119) :: LengthAtLeast 119

-- | Satisfies @length x >= 120@.
lengthAtLeast120 = LengthAtLeast (Proxy :: Proxy 120) :: LengthAtLeast 120

-- | Satisfies @length x >= 121@.
lengthAtLeast121 = LengthAtLeast (Proxy :: Proxy 121) :: LengthAtLeast 121

-- | Satisfies @length x >= 122@.
lengthAtLeast122 = LengthAtLeast (Proxy :: Proxy 122) :: LengthAtLeast 122

-- | Satisfies @length x >= 123@.
lengthAtLeast123 = LengthAtLeast (Proxy :: Proxy 123) :: LengthAtLeast 123

-- | Satisfies @length x >= 124@.
lengthAtLeast124 = LengthAtLeast (Proxy :: Proxy 124) :: LengthAtLeast 124

-- | Satisfies @length x >= 125@.
lengthAtLeast125 = LengthAtLeast (Proxy :: Proxy 125) :: LengthAtLeast 125

-- | Satisfies @length x >= 126@.
lengthAtLeast126 = LengthAtLeast (Proxy :: Proxy 126) :: LengthAtLeast 126

-- | Satisfies @length x >= 127@.
lengthAtLeast127 = LengthAtLeast (Proxy :: Proxy 127) :: LengthAtLeast 127

-- | Satisfies @length x >= 128@.
lengthAtLeast128 = LengthAtLeast (Proxy :: Proxy 128) :: LengthAtLeast 128



-- | Satisfies @length x <= n@.
data LengthAtMost (n :: Nat) = LengthAtMost (Proxy n)
  deriving (Eq, Show, Typeable)

instance (KnownNat n) => StringProperty (LengthAtMost n) where
  validator (LengthAtMost !proxy) str =
    let k = natVal $! proxy in
    let m = genericLength str in
    if m <= k
      then Right ()
      else
        let
          err = "Length is " ++ show m ++ ", but expected at most " ++ show k
        in
          Left [validationError err []]



-- | Satisfies @length x <= 1@.
lengthAtMost1 = LengthAtMost (Proxy :: Proxy 1) :: LengthAtMost 1

-- | Satisfies @length x <= 2@.
lengthAtMost2 = LengthAtMost (Proxy :: Proxy 2) :: LengthAtMost 2

-- | Satisfies @length x <= 3@.
lengthAtMost3 = LengthAtMost (Proxy :: Proxy 3) :: LengthAtMost 3

-- | Satisfies @length x <= 4@.
lengthAtMost4 = LengthAtMost (Proxy :: Proxy 4) :: LengthAtMost 4

-- | Satisfies @length x <= 5@.
lengthAtMost5 = LengthAtMost (Proxy :: Proxy 5) :: LengthAtMost 5

-- | Satisfies @length x <= 6@.
lengthAtMost6 = LengthAtMost (Proxy :: Proxy 6) :: LengthAtMost 6

-- | Satisfies @length x <= 7@.
lengthAtMost7 = LengthAtMost (Proxy :: Proxy 7) :: LengthAtMost 7

-- | Satisfies @length x <= 8@.
lengthAtMost8 = LengthAtMost (Proxy :: Proxy 8) :: LengthAtMost 8

-- | Satisfies @length x <= 9@.
lengthAtMost9 = LengthAtMost (Proxy :: Proxy 9) :: LengthAtMost 9

-- | Satisfies @length x <= 10@.
lengthAtMost10 = LengthAtMost (Proxy :: Proxy 10) :: LengthAtMost 10

-- | Satisfies @length x <= 11@.
lengthAtMost11 = LengthAtMost (Proxy :: Proxy 11) :: LengthAtMost 11

-- | Satisfies @length x <= 12@.
lengthAtMost12 = LengthAtMost (Proxy :: Proxy 12) :: LengthAtMost 12

-- | Satisfies @length x <= 13@.
lengthAtMost13 = LengthAtMost (Proxy :: Proxy 13) :: LengthAtMost 13

-- | Satisfies @length x <= 14@.
lengthAtMost14 = LengthAtMost (Proxy :: Proxy 14) :: LengthAtMost 14

-- | Satisfies @length x <= 15@.
lengthAtMost15 = LengthAtMost (Proxy :: Proxy 15) :: LengthAtMost 15

-- | Satisfies @length x <= 16@.
lengthAtMost16 = LengthAtMost (Proxy :: Proxy 16) :: LengthAtMost 16

-- | Satisfies @length x <= 17@.
lengthAtMost17 = LengthAtMost (Proxy :: Proxy 17) :: LengthAtMost 17

-- | Satisfies @length x <= 18@.
lengthAtMost18 = LengthAtMost (Proxy :: Proxy 18) :: LengthAtMost 18

-- | Satisfies @length x <= 19@.
lengthAtMost19 = LengthAtMost (Proxy :: Proxy 19) :: LengthAtMost 19

-- | Satisfies @length x <= 20@.
lengthAtMost20 = LengthAtMost (Proxy :: Proxy 20) :: LengthAtMost 20

-- | Satisfies @length x <= 21@.
lengthAtMost21 = LengthAtMost (Proxy :: Proxy 21) :: LengthAtMost 21

-- | Satisfies @length x <= 22@.
lengthAtMost22 = LengthAtMost (Proxy :: Proxy 22) :: LengthAtMost 22

-- | Satisfies @length x <= 23@.
lengthAtMost23 = LengthAtMost (Proxy :: Proxy 23) :: LengthAtMost 23

-- | Satisfies @length x <= 24@.
lengthAtMost24 = LengthAtMost (Proxy :: Proxy 24) :: LengthAtMost 24

-- | Satisfies @length x <= 25@.
lengthAtMost25 = LengthAtMost (Proxy :: Proxy 25) :: LengthAtMost 25

-- | Satisfies @length x <= 26@.
lengthAtMost26 = LengthAtMost (Proxy :: Proxy 26) :: LengthAtMost 26

-- | Satisfies @length x <= 27@.
lengthAtMost27 = LengthAtMost (Proxy :: Proxy 27) :: LengthAtMost 27

-- | Satisfies @length x <= 28@.
lengthAtMost28 = LengthAtMost (Proxy :: Proxy 28) :: LengthAtMost 28

-- | Satisfies @length x <= 29@.
lengthAtMost29 = LengthAtMost (Proxy :: Proxy 29) :: LengthAtMost 29

-- | Satisfies @length x <= 30@.
lengthAtMost30 = LengthAtMost (Proxy :: Proxy 30) :: LengthAtMost 30

-- | Satisfies @length x <= 31@.
lengthAtMost31 = LengthAtMost (Proxy :: Proxy 31) :: LengthAtMost 31

-- | Satisfies @length x <= 32@.
lengthAtMost32 = LengthAtMost (Proxy :: Proxy 32) :: LengthAtMost 32

-- | Satisfies @length x <= 33@.
lengthAtMost33 = LengthAtMost (Proxy :: Proxy 33) :: LengthAtMost 33

-- | Satisfies @length x <= 34@.
lengthAtMost34 = LengthAtMost (Proxy :: Proxy 34) :: LengthAtMost 34

-- | Satisfies @length x <= 35@.
lengthAtMost35 = LengthAtMost (Proxy :: Proxy 35) :: LengthAtMost 35

-- | Satisfies @length x <= 36@.
lengthAtMost36 = LengthAtMost (Proxy :: Proxy 36) :: LengthAtMost 36

-- | Satisfies @length x <= 37@.
lengthAtMost37 = LengthAtMost (Proxy :: Proxy 37) :: LengthAtMost 37

-- | Satisfies @length x <= 38@.
lengthAtMost38 = LengthAtMost (Proxy :: Proxy 38) :: LengthAtMost 38

-- | Satisfies @length x <= 39@.
lengthAtMost39 = LengthAtMost (Proxy :: Proxy 39) :: LengthAtMost 39

-- | Satisfies @length x <= 40@.
lengthAtMost40 = LengthAtMost (Proxy :: Proxy 40) :: LengthAtMost 40

-- | Satisfies @length x <= 41@.
lengthAtMost41 = LengthAtMost (Proxy :: Proxy 41) :: LengthAtMost 41

-- | Satisfies @length x <= 42@.
lengthAtMost42 = LengthAtMost (Proxy :: Proxy 42) :: LengthAtMost 42

-- | Satisfies @length x <= 43@.
lengthAtMost43 = LengthAtMost (Proxy :: Proxy 43) :: LengthAtMost 43

-- | Satisfies @length x <= 44@.
lengthAtMost44 = LengthAtMost (Proxy :: Proxy 44) :: LengthAtMost 44

-- | Satisfies @length x <= 45@.
lengthAtMost45 = LengthAtMost (Proxy :: Proxy 45) :: LengthAtMost 45

-- | Satisfies @length x <= 46@.
lengthAtMost46 = LengthAtMost (Proxy :: Proxy 46) :: LengthAtMost 46

-- | Satisfies @length x <= 47@.
lengthAtMost47 = LengthAtMost (Proxy :: Proxy 47) :: LengthAtMost 47

-- | Satisfies @length x <= 48@.
lengthAtMost48 = LengthAtMost (Proxy :: Proxy 48) :: LengthAtMost 48

-- | Satisfies @length x <= 49@.
lengthAtMost49 = LengthAtMost (Proxy :: Proxy 49) :: LengthAtMost 49

-- | Satisfies @length x <= 50@.
lengthAtMost50 = LengthAtMost (Proxy :: Proxy 50) :: LengthAtMost 50

-- | Satisfies @length x <= 51@.
lengthAtMost51 = LengthAtMost (Proxy :: Proxy 51) :: LengthAtMost 51

-- | Satisfies @length x <= 52@.
lengthAtMost52 = LengthAtMost (Proxy :: Proxy 52) :: LengthAtMost 52

-- | Satisfies @length x <= 53@.
lengthAtMost53 = LengthAtMost (Proxy :: Proxy 53) :: LengthAtMost 53

-- | Satisfies @length x <= 54@.
lengthAtMost54 = LengthAtMost (Proxy :: Proxy 54) :: LengthAtMost 54

-- | Satisfies @length x <= 55@.
lengthAtMost55 = LengthAtMost (Proxy :: Proxy 55) :: LengthAtMost 55

-- | Satisfies @length x <= 56@.
lengthAtMost56 = LengthAtMost (Proxy :: Proxy 56) :: LengthAtMost 56

-- | Satisfies @length x <= 57@.
lengthAtMost57 = LengthAtMost (Proxy :: Proxy 57) :: LengthAtMost 57

-- | Satisfies @length x <= 58@.
lengthAtMost58 = LengthAtMost (Proxy :: Proxy 58) :: LengthAtMost 58

-- | Satisfies @length x <= 59@.
lengthAtMost59 = LengthAtMost (Proxy :: Proxy 59) :: LengthAtMost 59

-- | Satisfies @length x <= 60@.
lengthAtMost60 = LengthAtMost (Proxy :: Proxy 60) :: LengthAtMost 60

-- | Satisfies @length x <= 61@.
lengthAtMost61 = LengthAtMost (Proxy :: Proxy 61) :: LengthAtMost 61

-- | Satisfies @length x <= 62@.
lengthAtMost62 = LengthAtMost (Proxy :: Proxy 62) :: LengthAtMost 62

-- | Satisfies @length x <= 63@.
lengthAtMost63 = LengthAtMost (Proxy :: Proxy 63) :: LengthAtMost 63

-- | Satisfies @length x <= 64@.
lengthAtMost64 = LengthAtMost (Proxy :: Proxy 64) :: LengthAtMost 64

-- | Satisfies @length x <= 65@.
lengthAtMost65 = LengthAtMost (Proxy :: Proxy 65) :: LengthAtMost 65

-- | Satisfies @length x <= 66@.
lengthAtMost66 = LengthAtMost (Proxy :: Proxy 66) :: LengthAtMost 66

-- | Satisfies @length x <= 67@.
lengthAtMost67 = LengthAtMost (Proxy :: Proxy 67) :: LengthAtMost 67

-- | Satisfies @length x <= 68@.
lengthAtMost68 = LengthAtMost (Proxy :: Proxy 68) :: LengthAtMost 68

-- | Satisfies @length x <= 69@.
lengthAtMost69 = LengthAtMost (Proxy :: Proxy 69) :: LengthAtMost 69

-- | Satisfies @length x <= 70@.
lengthAtMost70 = LengthAtMost (Proxy :: Proxy 70) :: LengthAtMost 70

-- | Satisfies @length x <= 71@.
lengthAtMost71 = LengthAtMost (Proxy :: Proxy 71) :: LengthAtMost 71

-- | Satisfies @length x <= 72@.
lengthAtMost72 = LengthAtMost (Proxy :: Proxy 72) :: LengthAtMost 72

-- | Satisfies @length x <= 73@.
lengthAtMost73 = LengthAtMost (Proxy :: Proxy 73) :: LengthAtMost 73

-- | Satisfies @length x <= 74@.
lengthAtMost74 = LengthAtMost (Proxy :: Proxy 74) :: LengthAtMost 74

-- | Satisfies @length x <= 75@.
lengthAtMost75 = LengthAtMost (Proxy :: Proxy 75) :: LengthAtMost 75

-- | Satisfies @length x <= 76@.
lengthAtMost76 = LengthAtMost (Proxy :: Proxy 76) :: LengthAtMost 76

-- | Satisfies @length x <= 77@.
lengthAtMost77 = LengthAtMost (Proxy :: Proxy 77) :: LengthAtMost 77

-- | Satisfies @length x <= 78@.
lengthAtMost78 = LengthAtMost (Proxy :: Proxy 78) :: LengthAtMost 78

-- | Satisfies @length x <= 79@.
lengthAtMost79 = LengthAtMost (Proxy :: Proxy 79) :: LengthAtMost 79

-- | Satisfies @length x <= 80@.
lengthAtMost80 = LengthAtMost (Proxy :: Proxy 80) :: LengthAtMost 80

-- | Satisfies @length x <= 81@.
lengthAtMost81 = LengthAtMost (Proxy :: Proxy 81) :: LengthAtMost 81

-- | Satisfies @length x <= 82@.
lengthAtMost82 = LengthAtMost (Proxy :: Proxy 82) :: LengthAtMost 82

-- | Satisfies @length x <= 83@.
lengthAtMost83 = LengthAtMost (Proxy :: Proxy 83) :: LengthAtMost 83

-- | Satisfies @length x <= 84@.
lengthAtMost84 = LengthAtMost (Proxy :: Proxy 84) :: LengthAtMost 84

-- | Satisfies @length x <= 85@.
lengthAtMost85 = LengthAtMost (Proxy :: Proxy 85) :: LengthAtMost 85

-- | Satisfies @length x <= 86@.
lengthAtMost86 = LengthAtMost (Proxy :: Proxy 86) :: LengthAtMost 86

-- | Satisfies @length x <= 87@.
lengthAtMost87 = LengthAtMost (Proxy :: Proxy 87) :: LengthAtMost 87

-- | Satisfies @length x <= 88@.
lengthAtMost88 = LengthAtMost (Proxy :: Proxy 88) :: LengthAtMost 88

-- | Satisfies @length x <= 89@.
lengthAtMost89 = LengthAtMost (Proxy :: Proxy 89) :: LengthAtMost 89

-- | Satisfies @length x <= 90@.
lengthAtMost90 = LengthAtMost (Proxy :: Proxy 90) :: LengthAtMost 90

-- | Satisfies @length x <= 91@.
lengthAtMost91 = LengthAtMost (Proxy :: Proxy 91) :: LengthAtMost 91

-- | Satisfies @length x <= 92@.
lengthAtMost92 = LengthAtMost (Proxy :: Proxy 92) :: LengthAtMost 92

-- | Satisfies @length x <= 93@.
lengthAtMost93 = LengthAtMost (Proxy :: Proxy 93) :: LengthAtMost 93

-- | Satisfies @length x <= 94@.
lengthAtMost94 = LengthAtMost (Proxy :: Proxy 94) :: LengthAtMost 94

-- | Satisfies @length x <= 95@.
lengthAtMost95 = LengthAtMost (Proxy :: Proxy 95) :: LengthAtMost 95

-- | Satisfies @length x <= 96@.
lengthAtMost96 = LengthAtMost (Proxy :: Proxy 96) :: LengthAtMost 96

-- | Satisfies @length x <= 97@.
lengthAtMost97 = LengthAtMost (Proxy :: Proxy 97) :: LengthAtMost 97

-- | Satisfies @length x <= 98@.
lengthAtMost98 = LengthAtMost (Proxy :: Proxy 98) :: LengthAtMost 98

-- | Satisfies @length x <= 99@.
lengthAtMost99 = LengthAtMost (Proxy :: Proxy 99) :: LengthAtMost 99

-- | Satisfies @length x <= 100@.
lengthAtMost100 = LengthAtMost (Proxy :: Proxy 100) :: LengthAtMost 100

-- | Satisfies @length x <= 101@.
lengthAtMost101 = LengthAtMost (Proxy :: Proxy 101) :: LengthAtMost 101

-- | Satisfies @length x <= 102@.
lengthAtMost102 = LengthAtMost (Proxy :: Proxy 102) :: LengthAtMost 102

-- | Satisfies @length x <= 103@.
lengthAtMost103 = LengthAtMost (Proxy :: Proxy 103) :: LengthAtMost 103

-- | Satisfies @length x <= 104@.
lengthAtMost104 = LengthAtMost (Proxy :: Proxy 104) :: LengthAtMost 104

-- | Satisfies @length x <= 105@.
lengthAtMost105 = LengthAtMost (Proxy :: Proxy 105) :: LengthAtMost 105

-- | Satisfies @length x <= 106@.
lengthAtMost106 = LengthAtMost (Proxy :: Proxy 106) :: LengthAtMost 106

-- | Satisfies @length x <= 107@.
lengthAtMost107 = LengthAtMost (Proxy :: Proxy 107) :: LengthAtMost 107

-- | Satisfies @length x <= 108@.
lengthAtMost108 = LengthAtMost (Proxy :: Proxy 108) :: LengthAtMost 108

-- | Satisfies @length x <= 109@.
lengthAtMost109 = LengthAtMost (Proxy :: Proxy 109) :: LengthAtMost 109

-- | Satisfies @length x <= 110@.
lengthAtMost110 = LengthAtMost (Proxy :: Proxy 110) :: LengthAtMost 110

-- | Satisfies @length x <= 111@.
lengthAtMost111 = LengthAtMost (Proxy :: Proxy 111) :: LengthAtMost 111

-- | Satisfies @length x <= 112@.
lengthAtMost112 = LengthAtMost (Proxy :: Proxy 112) :: LengthAtMost 112

-- | Satisfies @length x <= 113@.
lengthAtMost113 = LengthAtMost (Proxy :: Proxy 113) :: LengthAtMost 113

-- | Satisfies @length x <= 114@.
lengthAtMost114 = LengthAtMost (Proxy :: Proxy 114) :: LengthAtMost 114

-- | Satisfies @length x <= 115@.
lengthAtMost115 = LengthAtMost (Proxy :: Proxy 115) :: LengthAtMost 115

-- | Satisfies @length x <= 116@.
lengthAtMost116 = LengthAtMost (Proxy :: Proxy 116) :: LengthAtMost 116

-- | Satisfies @length x <= 117@.
lengthAtMost117 = LengthAtMost (Proxy :: Proxy 117) :: LengthAtMost 117

-- | Satisfies @length x <= 118@.
lengthAtMost118 = LengthAtMost (Proxy :: Proxy 118) :: LengthAtMost 118

-- | Satisfies @length x <= 119@.
lengthAtMost119 = LengthAtMost (Proxy :: Proxy 119) :: LengthAtMost 119

-- | Satisfies @length x <= 120@.
lengthAtMost120 = LengthAtMost (Proxy :: Proxy 120) :: LengthAtMost 120

-- | Satisfies @length x <= 121@.
lengthAtMost121 = LengthAtMost (Proxy :: Proxy 121) :: LengthAtMost 121

-- | Satisfies @length x <= 122@.
lengthAtMost122 = LengthAtMost (Proxy :: Proxy 122) :: LengthAtMost 122

-- | Satisfies @length x <= 123@.
lengthAtMost123 = LengthAtMost (Proxy :: Proxy 123) :: LengthAtMost 123

-- | Satisfies @length x <= 124@.
lengthAtMost124 = LengthAtMost (Proxy :: Proxy 124) :: LengthAtMost 124

-- | Satisfies @length x <= 125@.
lengthAtMost125 = LengthAtMost (Proxy :: Proxy 125) :: LengthAtMost 125

-- | Satisfies @length x <= 126@.
lengthAtMost126 = LengthAtMost (Proxy :: Proxy 126) :: LengthAtMost 126

-- | Satisfies @length x <= 127@.
lengthAtMost127 = LengthAtMost (Proxy :: Proxy 127) :: LengthAtMost 127

-- | Satisfies @length x <= 128@.
lengthAtMost128 = LengthAtMost (Proxy :: Proxy 128) :: LengthAtMost 128
