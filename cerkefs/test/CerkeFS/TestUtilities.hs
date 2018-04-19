module CerkeFS.TestUtilities
(shouldBeGivenBy
,shouldBeThrownBy
,shouldBeGeneratedBy
,res000, res001, res002, res003, res004, res005
)where
import Test.Hspec
import CerkeFS


shouldBeGivenBy b a = playFromStart a `shouldBe` b
shouldBeThrownBy b a = playFromStart a `shouldBe` Left b

shouldBeGeneratedBy b a = toDebugOutput a `shouldBe` b


res000, res001, res002, res003, res004, res005 :: String
res000 = 
 "_6h_5h_3h -  -  - _3k - _6k\n\
 \_7k_2k - _4k_8k_4h - _2h_7h\n\
 \_1h_1k_8h_1k_!k_1k_1h_1k_1h\n\
 \ -  - _1h -  -  -  -  -  - \n\
 \ - ^2h - ^4h^$h -  -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \^1h^1k^1h^1k^!h^1k^1h^1k^1h\n\
 \^7h -  -  -  - ^4k - ^2k^7k\n\
 \^6k^5k^3k^8k^#h^8h^3h^5h^6h\n\
 \~~~\n\
 \_5k^#k\n"
res001 = 
 "_6h_5h - _8h_#k_8k_3k_5k_6k\n\
 \ - _2k -  - _2h -  -  - _7h\n\
 \_1h_1k - _1k -  - _7k_1h_1h\n\
 \ -  - _1h -  - _1k -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  - ^1k - ^1h -  - \n\
 \^1h^1k^1h^4h -  - ^8h^1k^1h\n\
 \^7h^2h -  - ^$h^4k - ^3h^7k\n\
 \^6k - ^3k^8k^#h -  - ^5h^6h\n\
 \~~~\n\
 \_3h_1k_5k^1k^!k_!h^4k_2k_4h\n"
res002 = 
 " -  - _3h - _#k - _3k - _6k\n\
 \ -  -  - _4k_2k -  - _2h - \n\
 \_1h - _7k_1k - _1k_7h_1k_1h\n\
 \_6h - _1h -  - ^1k_1h -  - \n\
 \ - ^8k - ^1k -  -  -  -  - \n\
 \ -  - ^1h -  -  - ^1h -  - \n\
 \^1h -  -  -  - ^8h^!h^1k^1h\n\
 \ - ^2h -  - ^$h^4k^#h^2k^7k\n\
 \^6k^5k^3k -  -  - ^3h^5h^6h\n\
 \~~~\n\
 \^4h_7h^8k^1k_4h^8h^!k_1k^5h^5k\n"
res003 = 
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n\
 \ -  -  - _4k - _4h - _2h_7h\n\
 \_1h_1k_7k -  -  - _1h_1k_1h\n\
 \ -  - _1h_1k - _1k -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  -  -  - ^1h^1k - \n\
 \^1h^1k^1h^1k - ^1k^8h - ^1h\n\
 \^7h^2h - ^4h - ^4k^$h^2k^7k\n\
 \^6k^5k^3k^8k_2k - ^3h^5h^6h\n\
 \~~~\n\
 \_#h^!k_!h\n"
res004 = 
 "_6h_5h_3h -  -  - _3k_5k_6k\n\
 \_7k - _#k_4k_2k_8h -  - _7h\n\
 \_1h_1k_1h_1k_8k^$h_1h_1k_1h\n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ -  - ^1h^1k^2k - ^1h^1k^7k\n\
 \^1h^1k^4h - ^3h -  -  - ^1h\n\
 \^7h^2h -  -  - ^8h -  -  - \n\
 \^6k^5k^3k^8k_2h -  -  - _4h\n\
 \~~~\n\
 \_#h_5h^!k_!h_6h_4k^1k_1k\n"
res005 = 
 "_6h_5h_3h_8h_#k_8k_3k_5k_6k\n\
 \_7k -  - _2k - _4h^$h_7h - \n\
 \_1h_1k_1h -  - _1k_1h - _1h\n\
 \ -  -  -  - ^!h -  - _1k - \n\
 \ -  -  -  -  -  -  -  -  - \n\
 \ - ^2h -  -  -  -  -  -  - \n\
 \^1h^1k^1h_1k - ^1k^1h^1k^1h\n\
 \^7h -  -  - ^8h^4k - ^2k^7k\n\
 \^6k - ^3k^8k - ^#h^3h^5h^6h\n\
 \~~~\n\
 \_5k^!k_4h^2h_1k^4k\n"
