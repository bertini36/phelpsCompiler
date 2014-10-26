package Phelps_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-3, 1),(-2, 3)
-- State  1

-- State  2
,(-4, 5)
-- State  3

-- State  4
,(-7, 8)

-- State  5

-- State  6

-- State  7
,(-9, 11),(-8, 10)
-- State  8

-- State  9
,(-5, 13)
-- State  10

-- State  11

-- State  12

-- State  13
,(-20, 24)
,(-19, 23),(-18, 22),(-15, 21),(-14, 19)
,(-13, 18),(-12, 17),(-11, 28),(-3, 20)

-- State  14

-- State  15
,(-9, 29)
-- State  16
,(-10, 32)
-- State  17

-- State  18

-- State  19

-- State  20

-- State  21

-- State  22

-- State  23

-- State  24

-- State  25

-- State  26

-- State  27
,(-29, 44),(-27, 41)
,(-26, 40),(-25, 39),(-24, 38),(-23, 37)
,(-6, 36)
-- State  28

-- State  29

-- State  30

-- State  31

-- State  32

-- State  33

-- State  34

-- State  35

-- State  36
,(-29, 44),(-27, 41),(-26, 40)
,(-25, 39),(-24, 38),(-23, 53)
-- State  37

-- State  38

-- State  39

-- State  40

-- State  41

-- State  42
,(-29, 59)
,(-28, 55)
-- State  43
,(-29, 59),(-28, 60)
-- State  44

-- State  45
,(-30, 63)

-- State  46

-- State  47

-- State  48

-- State  49

-- State  50

-- State  51

-- State  52

-- State  53

-- State  54
,(-29, 59),(-28, 70)
-- State  55

-- State  56
,(-29, 59),(-28, 80)

-- State  57
,(-29, 59),(-28, 81)
-- State  58

-- State  59

-- State  60

-- State  61
,(-29, 59),(-28, 83)

-- State  62

-- State  63
,(-31, 86)
-- State  64

-- State  65

-- State  66

-- State  67
,(-22, 90),(-21, 89)
-- State  68

-- State  69

-- State  70

-- State  71
,(-29, 44)
,(-27, 41),(-26, 40),(-25, 39),(-24, 38)
,(-23, 37),(-6, 94)
-- State  72
,(-29, 59),(-28, 95)

-- State  73
,(-29, 59),(-28, 96)
-- State  74
,(-29, 59),(-28, 97)

-- State  75
,(-29, 59),(-28, 98)
-- State  76
,(-29, 59),(-28, 99)

-- State  77
,(-29, 59),(-28, 100)
-- State  78
,(-29, 59),(-28, 101)

-- State  79
,(-29, 59),(-28, 102)
-- State  80

-- State  81

-- State  82
,(-29, 44),(-27, 41)
,(-26, 40),(-25, 39),(-24, 38),(-23, 37)
,(-6, 104)
-- State  83

-- State  84

-- State  85
,(-32, 107),(-29, 59),(-28, 108)

-- State  86

-- State  87
,(-17, 110),(-16, 113)
-- State  88

-- State  89
,(-22, 116)
-- State  90

-- State  91

-- State  92
,(-15, 118)

-- State  93

-- State  94
,(-29, 44),(-27, 41),(-26, 40),(-25, 39)
,(-24, 38),(-23, 53)
-- State  95

-- State  96

-- State  97

-- State  98

-- State  99

-- State  100

-- State  101

-- State  102

-- State  103

-- State  104
,(-29, 44),(-27, 41)
,(-26, 40),(-25, 39),(-24, 38),(-23, 53)

-- State  105

-- State  106

-- State  107

-- State  108

-- State  109
,(-17, 124)
-- State  110

-- State  111

-- State  112

-- State  113

-- State  114
,(-17, 110),(-16, 126)
-- State  115

-- State  116

-- State  117

-- State  118

-- State  119

-- State  120
,(-29, 44)
,(-27, 41),(-26, 40),(-25, 39),(-24, 38)
,(-23, 37),(-6, 131)
-- State  121

-- State  122

-- State  123
,(-29, 59),(-28, 133)

-- State  124

-- State  125

-- State  126

-- State  127

-- State  128

-- State  129

-- State  130

-- State  131
,(-29, 44),(-27, 41),(-26, 40),(-25, 39)
,(-24, 38),(-23, 53)
-- State  132

-- State  133

-- State  134
,(-17, 110),(-16, 138)

-- State  135

-- State  136

-- State  137

-- State  138

-- State  139

-- State  140

-- State  141

-- State  142

);
--  The offset vector
GOTO_OFFSET : array (0.. 142) of Integer :=
( 0,
 2, 2, 3, 3, 4, 4, 4, 6, 6, 7,
 7, 7, 7, 16, 16, 17, 18, 18, 18, 18,
 18, 18, 18, 18, 18, 18, 18, 25, 25, 25,
 25, 25, 25, 25, 25, 25, 31, 31, 31, 31,
 31, 31, 33, 35, 35, 36, 36, 36, 36, 36,
 36, 36, 36, 36, 38, 38, 40, 42, 42, 42,
 42, 44, 44, 45, 45, 45, 45, 47, 47, 47,
 47, 54, 56, 58, 60, 62, 64, 66, 68, 70,
 70, 70, 77, 77, 77, 80, 80, 82, 82, 83,
 83, 83, 84, 84, 90, 90, 90, 90, 90, 90,
 90, 90, 90, 90, 96, 96, 96, 96, 96, 97,
 97, 97, 97, 97, 99, 99, 99, 99, 99, 99,
 106, 106, 106, 108, 108, 108, 108, 108, 108, 108,
 108, 114, 114, 114, 116, 116, 116, 116, 116, 116,
 116, 116);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  65) of Natural := ( 2,
 1, 9, 2, 3, 0, 3, 1, 4,
 1, 1, 2, 2, 0, 1, 1, 1,
 1, 4, 3, 1, 7, 2, 1, 1,
 1, 1, 1, 1, 10, 7, 10, 2,
 1, 4, 2, 1, 1, 1, 1, 1,
 6, 8, 6, 4, 2, 2, 2, 0,
 2, 3, 3, 1, 2, 3, 3, 3,
 3, 3, 2, 3, 3, 3, 3, 1,
 1);
   Get_LHS_Rule: array (Rule range  0 ..  65) of Nonterminal := (-1,
-2,-3,-4,-7,-7,-8,-8,-9,
-10,-10,-10,-5,-5,-11,-11,-11,
-11,-12,-15,-15,-13,-16,-16,-17,
-17,-14,-14,-14,-18,-19,-20,-21,
-21,-22,-6,-6,-23,-23,-23,-23,
-24,-24,-25,-26,-27,-29,-30,-30,
-31,-31,-32,-32,-28,-28,-28,-28,
-28,-28,-28,-28,-28,-28,-28,-28,
-28);
end Phelps_Goto;
