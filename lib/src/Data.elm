module Data exposing (..)

import Dict exposing (Dict)


{-| standardNormalDistoributionUpper
z≧Aのパターン
実は正規分布のこうしたパーセンテージを求めるやり方をイマイチ理解していない。
だからこうして仰々しいDictを作った。
-}
standardNormalDistoributionUpper : Dict Float (List Float)
standardNormalDistoributionUpper =
    Dict.fromList
        [ --'正規分布',z, 0,1,2,3,4,5,6,7,8,9
          ( 0, [ 0.5, 0.496011, 0.492022, 0.488033, 0.484047, 0.480061, 0.476078, 0.472097, 0.468119, 0.464144 ] )
        , ( 0.1, [ 0.460172, 0.456205, 0.452242, 0.448283, 0.44433, 0.440382, 0.436441, 0.432505, 0.428576, 0.424655 ] )
        , ( 0.2, [ 0.42074, 0.416834, 0.412936, 0.409046, 0.405165, 0.401294, 0.397432, 0.39358, 0.389739, 0.385908 ] )
        , ( 0.3, [ 0.382089, 0.378281, 0.374484, 0.3707, 0.366928, 0.363169, 0.359424, 0.355691, 0.351973, 0.348268 ] )
        , ( 0.4, [ 0.344578, 0.340903, 0.337243, 0.333598, 0.329969, 0.326355, 0.322758, 0.319178, 0.315614, 0.312067 ] )
        , ( 0.5, [ 0.308538, 0.305026, 0.301532, 0.298056, 0.294598, 0.29116, 0.28774, 0.284339, 0.280957, 0.277595 ] )
        , ( 0.6, [ 0.274253, 0.270931, 0.267629, 0.264347, 0.261086, 0.257846, 0.254627, 0.251429, 0.248252, 0.245097 ] )
        , ( 0.7, [ 0.241964, 0.238852, 0.235762, 0.232695, 0.22965, 0.226627, 0.223627, 0.22065, 0.217695, 0.214764 ] )
        , ( 0.8, [ 0.211855, 0.20897, 0.206108, 0.203269, 0.200454, 0.197662, 0.194894, 0.19215, 0.18943, 0.186733 ] )
        , ( 0.9, [ 0.18406, 0.181411, 0.178786, 0.176186, 0.173609, 0.171056, 0.168528, 0.166023, 0.163543, 0.161087 ] )
        , ( 1, [ 0.158655, 0.156248, 0.153864, 0.151505, 0.14917, 0.146859, 0.144572, 0.14231, 0.140071, 0.137857 ] )
        , ( 1.1, [ 0.135666, 0.1335, 0.131357, 0.129238, 0.127143, 0.125072, 0.123024, 0.121001, 0.119, 0.117023 ] )
        , ( 1.2, [ 0.11507, 0.11314, 0.111233, 0.109349, 0.107488, 0.10565, 0.103835, 0.102042, 0.100273, 0.098525 ] )
        , ( 1.3, [ 0.096801, 0.095098, 0.093418, 0.091759, 0.090123, 0.088508, 0.086915, 0.085344, 0.083793, 0.082264 ] )
        , ( 1.4, [ 0.080757, 0.07927, 0.077804, 0.076359, 0.074934, 0.073529, 0.072145, 0.070781, 0.069437, 0.068112 ] )
        , ( 1.5, [ 0.066807, 0.065522, 0.064256, 0.063008, 0.06178, 0.060571, 0.05938, 0.058208, 0.057053, 0.055917 ] )
        , ( 1.6, [ 0.054799, 0.053699, 0.052616, 0.051551, 0.050503, 0.049471, 0.048457, 0.04746, 0.046479, 0.045514 ] )
        , ( 1.7, [ 0.044565, 0.043633, 0.042716, 0.041815, 0.040929, 0.040059, 0.039204, 0.038364, 0.037538, 0.036727 ] )
        , ( 1.8, [ 0.03593, 0.035148, 0.034379, 0.033625, 0.032884, 0.032157, 0.031443, 0.030742, 0.030054, 0.029379 ] )
        , ( 1.9, [ 0.028716, 0.028067, 0.027429, 0.026803, 0.02619, 0.025588, 0.024998, 0.024419, 0.023852, 0.023295 ] )
        , ( 2, [ 0.02275, 0.022216, 0.021692, 0.021178, 0.020675, 0.020182, 0.019699, 0.019226, 0.018763, 0.018309 ] )
        , ( 2.1, [ 0.017864, 0.017429, 0.017003, 0.016586, 0.016177, 0.015778, 0.015386, 0.015003, 0.014629, 0.014262 ] )
        , ( 2.2, [ 0.013903, 0.013553, 0.013209, 0.012874, 0.012545, 0.012224, 0.011911, 0.011604, 0.011304, 0.011011 ] )
        , ( 2.3, [ 0.010724, 0.010444, 0.01017, 0.009903, 0.009642, 0.009387, 0.009137, 0.008894, 0.008656, 0.008424 ] )
        , ( 2.4, [ 0.008198, 0.007976, 0.00776, 0.007549, 0.007344, 0.007143, 0.006947, 0.006756, 0.006569, 0.006387 ] )
        , ( 2.5, [ 0.00621, 0.006037, 0.005868, 0.005703, 0.005543, 0.005386, 0.005234, 0.005085, 0.00494, 0.004799 ] )
        , ( 2.6, [ 0.004661, 0.004527, 0.004397, 0.004269, 0.004145, 0.004025, 0.003907, 0.003793, 0.003681, 0.003573 ] )
        , ( 2.7, [ 0.003467, 0.003364, 0.003264, 0.003167, 0.003072, 0.00298, 0.00289, 0.002803, 0.002718, 0.002635 ] )
        , ( 2.8, [ 0.002555, 0.002477, 0.002401, 0.002327, 0.002256, 0.002186, 0.002118, 0.002052, 0.001988, 0.001926 ] )
        , ( 2.9, [ 0.001866, 0.001807, 0.00175, 0.001695, 0.001641, 0.001589, 0.001538, 0.001489, 0.001441, 0.001395 ] )
        , ( 3, [ 0.00135, 0.001306, 0.001264, 0.001223, 0.001183, 0.001144, 0.001107, 0.00107, 0.001035, 0.001001 ] )
        , ( 3.1, [ 0.000968, 0.000936, 0.000904, 0.000874, 0.000845, 0.000816, 0.000789, 0.000762, 0.000736, 0.000711 ] )
        , ( 3.2, [ 0.000687, 0.000664, 0.000641, 0.000619, 0.000598, 0.000577, 0.000557, 0.000538, 0.000519, 0.000501 ] )
        , ( 3.3, [ 0.000483, 0.000467, 0.00045, 0.000434, 0.000419, 0.000404, 0.00039, 0.000376, 0.000362, 0.00035 ] )
        , ( 3.4, [ 0.000337, 0.000325, 0.000313, 0.000302, 0.000291, 0.00028, 0.00027, 0.00026, 0.000251, 0.000242 ] )
        , ( 3.5, [ 0.000233, 0.000224, 0.000216, 0.000208, 0.0002, 0.000193, 0.000185, 0.000179, 0.000172, 0.000165 ] )
        , ( 3.6, [ 0.000159, 0.000153, 0.000147, 0.000142, 0.000136, 0.000131, 0.000126, 0.000121, 0.000117, 0.000112 ] )
        , ( 3.7, [ 0.000108, 0.000104, 9.96e-5, 9.58e-5, 9.2e-5, 8.84e-5, 8.5e-5, 8.16e-5, 7.84e-5, 7.53e-5 ] )
        , ( 3.8, [ 7.24e-5, 6.95e-5, 6.67e-5, 6.41e-5, 6.15e-5, 5.91e-5, 5.67e-5, 5.44e-5, 5.22e-5, 5.01e-5 ] )
        , ( 3.9, [ 4.81e-5, 4.62e-5, 4.43e-5, 4.25e-5, 4.08e-5, 3.91e-5, 3.75e-5, 3.6e-5, 3.45e-5, 3.31e-5 ] )
        , ( 4, [ 3.17e-5, 3.04e-5, 2.91e-5, 2.79e-5, 2.67e-5, 2.56e-5, 2.45e-5, 2.35e-5, 2.25e-5, 2.16e-5 ] )
        , ( 4.1, [ 2.07e-5, 1.98e-5, 1.9e-5, 1.81e-5, 1.74e-5, 1.66e-5, 1.59e-5, 1.52e-5, 1.46e-5, 1.4e-5 ] )
        , ( 4.2, [ 1.34e-5, 1.28e-5, 1.22e-5, 1.17e-5, 1.12e-5, 1.07e-5, 1.02e-5, 9.78e-6, 9.35e-6, 8.94e-6 ] )
        , ( 4.3, [ 8.55e-6, 8.17e-6, 7.81e-6, 7.46e-6, 7.13e-6, 6.81e-6, 6.51e-6, 6.22e-6, 5.94e-6, 5.67e-6 ] )
        , ( 4.4, [ 5.42e-6, 5.17e-6, 4.94e-6, 4.72e-6, 4.5e-6, 4.3e-6, 4.1e-6, 3.91e-6, 3.74e-6, 3.56e-6 ] )
        , ( 4.5, [ 3.4e-6, 3.24e-6, 3.09e-6, 2.95e-6, 2.82e-6, 2.68e-6, 2.56e-6, 2.44e-6, 2.33e-6, 2.22e-6 ] )
        , ( 4.6, [ 2.11e-6, 2.02e-6, 1.92e-6, 1.83e-6, 1.74e-6, 1.66e-6, 1.58e-6, 1.51e-6, 1.44e-6, 1.37e-6 ] )
        , ( 4.7, [ 1.3e-6, 1.24e-6, 1.18e-6, 1.12e-6, 1.07e-6, 1.02e-6, 9.69e-7, 9.22e-7, 8.78e-7, 8.35e-7 ] )
        , ( 4.8, [ 7.94e-7, 7.56e-7, 7.19e-7, 6.84e-7, 6.5e-7, 6.18e-7, 5.88e-7, 5.59e-7, 5.31e-7, 5.05e-7 ] )
        , ( 4.9, [ 4.8e-7, 4.56e-7, 4.33e-7, 4.12e-7, 3.91e-7, 3.72e-7, 3.53e-7, 3.35e-7, 3.18e-7, 3.02e-7 ] )
        , ( 5, [ 2.87e-7, 2.73e-7, 2.59e-7, 2.46e-7, 2.33e-7, 2.21e-7, 2.1e-7, 1.99e-7, 1.89e-7, 1.79e-7 ] )
        ]


{-| tDistIntentionalLevelFivePer
t分布の5%の有意水準
自由度 t を受け取ってそれに対応したFloatを返す。
なんかすごいことになっちゃった。
書いてて思った。あの計算式の詳細を僕はまだ知らない。

    tDistIntentionalLevelFivePer 19

    OUT 2.093

-}
tDistIntentionalLevelFivePer : Int -> Float
tDistIntentionalLevelFivePer t =
    let
        --  信頼水準, [1-α=0.95,1-2×(α/2)=1-α=0.95,1-α=0.9545,1-2×(α/2)=1-α=0.9545,1-α=0.9973,1-2×(α/2)=1-α=0.9973]),
        --  危険率, [α=0.05,α/2=0.025,α=0.0455,α/2=0.02275,α=0.0027,α/2=0.00135]),
        -- 実はスクレイピングしたのは内緒
        tDict =
            Dict.fromList
                [ ( 1, [ 6.3137, 12.7062, 6.9481, 13.9678, 117.8868, 235.7736 ] )
                , ( 2, [ 2.92, 4.3027, 3.0843, 4.5266, 13.5531, 19.2062 ] )
                , ( 3, [ 2.3534, 3.1824, 2.4584, 3.3068, 7.2565, 9.2189 ] )
                , ( 4, [ 2.1318, 2.7765, 2.2162, 2.8693, 5.4797, 6.62 ] )
                , ( 5, [ 2.015, 2.5706, 2.0891, 2.6487, 4.6869, 5.507 ] )
                , ( 6, [ 1.9432, 2.4469, 2.0112, 2.5165, 4.2467, 4.904 ] )
                , ( 7, [ 1.8946, 2.3646, 1.9587, 2.4288, 3.9689, 4.5299 ] )
                , ( 8, [ 1.8595, 2.306, 1.9208, 2.3664, 3.7785, 4.2765 ] )
                , ( 9, [ 1.8331, 2.2622, 1.8923, 2.3198, 3.6401, 4.0942 ] )
                , ( 10, [ 1.8125, 2.2281, 1.8701, 2.2837, 3.5352, 3.957 ] )
                , ( 11, [ 1.7959, 2.201, 1.8522, 2.2549, 3.453, 3.8499 ] )
                , ( 12, [ 1.7823, 2.1788, 1.8376, 2.2314, 3.3868, 3.7643 ] )
                , ( 13, [ 1.7709, 2.1604, 1.8254, 2.2118, 3.3325, 3.6941 ] )
                , ( 14, [ 1.7613, 2.1448, 1.815, 2.1953, 3.287, 3.6358 ] )
                , ( 15, [ 1.7531, 2.1315, 1.8061, 2.1812, 3.2485, 3.5864 ] )
                , ( 16, [ 1.7459, 2.1199, 1.7984, 2.1689, 3.2154, 3.5441 ] )
                , ( 17, [ 1.7396, 2.1098, 1.7917, 2.1583, 3.1867, 3.5074 ] )
                , ( 18, [ 1.7341, 2.1009, 1.7857, 2.1489, 3.1615, 3.4754 ] )
                , ( 19, [ 1.7291, 2.093, 1.7804, 2.1405, 3.1393, 3.4472 ] )
                , ( 20, [ 1.7247, 2.086, 1.7757, 2.133, 3.1196, 3.4221 ] )
                , ( 21, [ 1.7207, 2.0796, 1.7714, 2.1263, 3.1018, 3.3997 ] )
                , ( 22, [ 1.7171, 2.0739, 1.7676, 2.1202, 3.0859, 3.3795 ] )
                , ( 23, [ 1.7139, 2.0687, 1.7641, 2.1147, 3.0715, 3.3613 ] )
                , ( 24, [ 1.7109, 2.0639, 1.7609, 2.1097, 3.0584, 3.3448 ] )
                , ( 25, [ 1.7081, 2.0595, 1.7579, 2.1051, 3.0464, 3.3296 ] )
                , ( 26, [ 1.7056, 2.0555, 1.7552, 2.1009, 3.0354, 3.3157 ] )
                , ( 27, [ 1.7033, 2.0518, 1.7527, 2.0969, 3.0253, 3.303 ] )
                , ( 28, [ 1.7011, 2.0484, 1.7504, 2.0933, 3.016, 3.2913 ] )
                , ( 29, [ 1.6991, 2.0452, 1.7483, 2.09, 3.0073, 3.2804 ] )
                , ( 30, [ 1.6973, 2.0423, 1.7463, 2.0868, 2.9993, 3.2703 ] )
                , ( 31, [ 1.6955, 2.0395, 1.7444, 2.0839, 2.9918, 3.2609 ] )
                , ( 32, [ 1.6939, 2.0369, 1.7426, 2.0812, 2.9848, 3.2521 ] )
                , ( 33, [ 1.6924, 2.0345, 1.741, 2.0787, 2.9783, 3.244 ] )
                , ( 34, [ 1.6909, 2.0322, 1.7395, 2.0763, 2.9722, 3.2363 ] )
                , ( 35, [ 1.6896, 2.0301, 1.738, 2.074, 2.9664, 3.2291 ] )
                , ( 36, [ 1.6883, 2.0281, 1.7367, 2.0719, 2.961, 3.2223 ] )
                , ( 37, [ 1.6871, 2.0262, 1.7354, 2.0699, 2.9559, 3.2159 ] )
                , ( 38, [ 1.686, 2.0244, 1.7341, 2.068, 2.9511, 3.2099 ] )
                , ( 39, [ 1.6849, 2.0227, 1.733, 2.0662, 2.9465, 3.2042 ] )
                , ( 40, [ 1.6839, 2.0211, 1.7319, 2.0645, 2.9422, 3.1987 ] )
                , ( 41, [ 1.6829, 2.0195, 1.7308, 2.0628, 2.9381, 3.1936 ] )
                , ( 42, [ 1.682, 2.0181, 1.7299, 2.0613, 2.9342, 3.1888 ] )
                , ( 43, [ 1.6811, 2.0167, 1.7289, 2.0598, 2.9305, 3.1841 ] )
                , ( 44, [ 1.6802, 2.0154, 1.728, 2.0584, 2.9269, 3.1797 ] )
                , ( 45, [ 1.6794, 2.0141, 1.7271, 2.0571, 2.9236, 3.1755 ] )
                , ( 46, [ 1.6787, 2.0129, 1.7263, 2.0558, 2.9204, 3.1715 ] )
                , ( 47, [ 1.6779, 2.0117, 1.7255, 2.0546, 2.9173, 3.1677 ] )
                , ( 48, [ 1.6772, 2.0106, 1.7248, 2.0534, 2.9143, 3.164 ] )
                , ( 49, [ 1.6766, 2.0096, 1.7241, 2.0523, 2.9115, 3.1605 ] )
                , ( 50, [ 1.6759, 2.0086, 1.7234, 2.0513, 2.9089, 3.1571 ] )
                , ( 51, [ 1.6753, 2.0076, 1.7227, 2.0502, 2.9062, 3.1539 ] )
                , ( 52, [ 1.6747, 2.0066, 1.7221, 2.0492, 2.9038, 3.1508 ] )
                , ( 53, [ 1.6741, 2.0057, 1.7215, 2.0483, 2.9014, 3.1479 ] )
                , ( 54, [ 1.6736, 2.0049, 1.7209, 2.0474, 2.8991, 3.145 ] )
                , ( 55, [ 1.673, 2.004, 1.7203, 2.0465, 2.8969, 3.1422 ] )
                , ( 56, [ 1.6725, 2.0032, 1.7198, 2.0456, 2.8948, 3.1396 ] )
                , ( 57, [ 1.672, 2.0025, 1.7192, 2.0448, 2.8927, 3.137 ] )
                , ( 58, [ 1.6716, 2.0017, 1.7187, 2.044, 2.8907, 3.1346 ] )
                , ( 59, [ 1.6711, 2.001, 1.7182, 2.0433, 2.8888, 3.1322 ] )
                , ( 60, [ 1.6706, 2.0003, 1.7177, 2.0425, 2.887, 3.1299 ] )
                , ( 61, [ 1.6702, 1.9996, 1.7173, 2.0418, 2.8852, 3.1277 ] )
                , ( 62, [ 1.6698, 1.999, 1.7168, 2.0411, 2.8835, 3.1255 ] )
                , ( 63, [ 1.6694, 1.9983, 1.7164, 2.0405, 2.8818, 3.1235 ] )
                , ( 64, [ 1.669, 1.9977, 1.716, 2.0398, 2.8802, 3.1215 ] )
                , ( 65, [ 1.6686, 1.9971, 1.7156, 2.0392, 2.8787, 3.1196 ] )
                , ( 66, [ 1.6683, 1.9966, 1.7152, 2.0386, 2.8772, 3.1177 ] )
                , ( 67, [ 1.6679, 1.996, 1.7148, 2.038, 2.8757, 3.1159 ] )
                , ( 68, [ 1.6676, 1.9955, 1.7145, 2.0374, 2.8743, 3.1141 ] )
                , ( 69, [ 1.6672, 1.9949, 1.7141, 2.0369, 2.8729, 3.1124 ] )
                , ( 70, [ 1.6669, 1.9944, 1.7138, 2.0363, 2.8716, 3.1108 ] )
                , ( 71, [ 1.6666, 1.9939, 1.7134, 2.0358, 2.8703, 3.1091 ] )
                , ( 72, [ 1.6663, 1.9935, 1.7131, 2.0353, 2.869, 3.1076 ] )
                , ( 73, [ 1.666, 1.993, 1.7128, 2.0348, 2.8678, 3.106 ] )
                , ( 74, [ 1.6657, 1.9925, 1.7125, 2.0344, 2.8666, 3.1046 ] )
                , ( 75, [ 1.6654, 1.9921, 1.7122, 2.0339, 2.8655, 3.1031 ] )
                , ( 76, [ 1.6652, 1.9917, 1.7119, 2.0334, 2.8643, 3.1017 ] )
                , ( 77, [ 1.6649, 1.9913, 1.7116, 2.033, 2.8632, 3.1004 ] )
                , ( 78, [ 1.6646, 1.9908, 1.7113, 2.0326, 2.8621, 3.099 ] )
                , ( 79, [ 1.6644, 1.9905, 1.711, 2.0321, 2.8611, 3.0977 ] )
                , ( 80, [ 1.6641, 1.9901, 1.7108, 2.0317, 2.8601, 3.0965 ] )
                , ( 81, [ 1.6639, 1.9897, 1.7105, 2.0313, 2.8591, 3.0953 ] )
                , ( 82, [ 1.6636, 1.9893, 1.7103, 2.031, 2.8581, 3.094 ] )
                , ( 83, [ 1.6634, 1.989, 1.71, 2.0306, 2.8572, 3.0929 ] )
                , ( 84, [ 1.6632, 1.9886, 1.7098, 2.0302, 2.8563, 3.0918 ] )
                , ( 85, [ 1.663, 1.9883, 1.7095, 2.0298, 2.8554, 3.0907 ] )
                , ( 86, [ 1.6628, 1.9879, 1.7093, 2.0295, 2.8545, 3.0896 ] )
                , ( 87, [ 1.6626, 1.9876, 1.7091, 2.0291, 2.8537, 3.0885 ] )
                , ( 88, [ 1.6624, 1.9873, 1.7089, 2.0288, 2.8529, 3.0875 ] )
                , ( 89, [ 1.6622, 1.987, 1.7087, 2.0285, 2.852, 3.0865 ] )
                , ( 90, [ 1.662, 1.9867, 1.7084, 2.0282, 2.8513, 3.0855 ] )
                , ( 91, [ 1.6618, 1.9864, 1.7082, 2.0279, 2.8505, 3.0845 ] )
                , ( 92, [ 1.6616, 1.9861, 1.708, 2.0275, 2.8497, 3.0836 ] )
                , ( 93, [ 1.6614, 1.9858, 1.7079, 2.0272, 2.849, 3.0827 ] )
                , ( 94, [ 1.6612, 1.9855, 1.7077, 2.0269, 2.8482, 3.0817 ] )
                , ( 95, [ 1.6611, 1.9852, 1.7075, 2.0267, 2.8475, 3.0809 ] )
                , ( 96, [ 1.6609, 1.985, 1.7073, 2.0264, 2.8468, 3.08 ] )
                , ( 97, [ 1.6607, 1.9847, 1.7071, 2.0261, 2.8462, 3.0792 ] )
                , ( 98, [ 1.6606, 1.9845, 1.7069, 2.0258, 2.8455, 3.0783 ] )
                , ( 99, [ 1.6604, 1.9842, 1.7068, 2.0256, 2.8448, 3.0775 ] )
                , ( 100, [ 1.6602, 1.984, 1.7066, 2.0253, 2.8442, 3.0767 ] )
                ]

        digreeOfFree =
            Maybe.withDefault [ 1.6602, 1.984, 1.7066, 2.0253, 2.8442, 3.0767 ] (Dict.get t tDict)
    in
    if t > 30 then
        1.96

    else
        Maybe.withDefault 1.984 (List.head (List.drop 1 digreeOfFree))


{-
カイ二乗分布表から一つの行を取り出す関数
-}
pickUpChi:Int -> Dict Float Float
pickUpChi n =
    let
        chiSquareDistribution : Dict Int (Dict Float Float)
        chiSquareDistribution =
            Dict.fromList
                [ ( 1, Dict.fromList [ ( 0.995, 0.00004 ), ( 0.99, 0.00016 ), ( 0.975, 0.00098 ), ( 0.95, 0.00393 ), ( 0.9, 0.01579 ), ( 0.5, 0.45494 ), ( 0.1, 2.7055 ), ( 0.05, 3.8415 ), ( 0.025, 5.0239 ), ( 0.01, 6.6349 ), ( 0.005, 7.8794 ) ] )
                , ( 2, Dict.fromList [ ( 0.995, 0.01003 ), ( 0.99, 0.0201 ), ( 0.975, 0.05064 ), ( 0.95, 0.10259 ), ( 0.9, 0.21072 ), ( 0.5, 1.3863 ), ( 0.1, 4.6052 ), ( 0.05, 5.9915 ), ( 0.025, 7.3778 ), ( 0.01, 9.2103 ), ( 0.005, 10.597 ) ] )
                , ( 3, Dict.fromList [ ( 0.995, 0.07172 ), ( 0.99, 0.11483 ), ( 0.975, 0.2158 ), ( 0.95, 0.35185 ), ( 0.9, 0.58437 ), ( 0.5, 2.366 ), ( 0.1, 6.2514 ), ( 0.05, 7.8147 ), ( 0.025, 9.3484 ), ( 0.01, 11.345 ), ( 0.005, 12.838 ) ] )
                , ( 4, Dict.fromList [ ( 0.995, 0.20699 ), ( 0.99, 0.29711 ), ( 0.975, 0.48442 ), ( 0.95, 0.71072 ), ( 0.9, 1.0636 ), ( 0.5, 3.3567 ), ( 0.1, 7.7794 ), ( 0.05, 9.4877 ), ( 0.025, 11.143 ), ( 0.01, 13.277 ), ( 0.005, 14.86 ) ] )
                , ( 5, Dict.fromList [ ( 0.995, 0.41174 ), ( 0.99, 0.5543 ), ( 0.975, 0.83121 ), ( 0.95, 1.1455 ), ( 0.9, 1.6103 ), ( 0.5, 4.3515 ), ( 0.1, 9.2364 ), ( 0.05, 11.07 ), ( 0.025, 12.833 ), ( 0.01, 15.086 ), ( 0.005, 16.75 ) ] )
                , ( 6, Dict.fromList [ ( 0.995, 0.67573 ), ( 0.99, 0.87209 ), ( 0.975, 1.2373 ), ( 0.95, 1.6354 ), ( 0.9, 2.2041 ), ( 0.5, 5.3481 ), ( 0.1, 10.645 ), ( 0.05, 12.592 ), ( 0.025, 14.449 ), ( 0.01, 16.812 ), ( 0.005, 18.548 ) ] )
                , ( 7, Dict.fromList [ ( 0.995, 0.98926 ), ( 0.99, 1.239 ), ( 0.975, 1.6899 ), ( 0.95, 2.1673 ), ( 0.9, 2.8331 ), ( 0.5, 6.3458 ), ( 0.1, 12.017 ), ( 0.05, 14.067 ), ( 0.025, 16.013 ), ( 0.01, 18.475 ), ( 0.005, 20.278 ) ] )
                , ( 8, Dict.fromList [ ( 0.995, 1.3444 ), ( 0.99, 1.6465 ), ( 0.975, 2.1797 ), ( 0.95, 2.7326 ), ( 0.9, 3.4895 ), ( 0.5, 7.3441 ), ( 0.1, 13.362 ), ( 0.05, 15.507 ), ( 0.025, 17.535 ), ( 0.01, 20.09 ), ( 0.005, 21.955 ) ] )
                , ( 9, Dict.fromList [ ( 0.995, 1.7349 ), ( 0.99, 2.0879 ), ( 0.975, 2.7004 ), ( 0.95, 3.3251 ), ( 0.9, 4.1682 ), ( 0.5, 8.3428 ), ( 0.1, 14.684 ), ( 0.05, 16.919 ), ( 0.025, 19.023 ), ( 0.01, 21.666 ), ( 0.005, 23.589 ) ] )
                , ( 10, Dict.fromList [ ( 0.995, 2.1559 ), ( 0.99, 2.5582 ), ( 0.975, 3.247 ), ( 0.95, 3.9403 ), ( 0.9, 4.8652 ), ( 0.5, 9.3418 ), ( 0.1, 15.987 ), ( 0.05, 18.307 ), ( 0.025, 20.483 ), ( 0.01, 23.209 ), ( 0.005, 25.188 ) ] )
                , ( 11, Dict.fromList [ ( 0.995, 2.6032 ), ( 0.99, 3.0535 ), ( 0.975, 3.8157 ), ( 0.95, 4.5748 ), ( 0.9, 5.5778 ), ( 0.5, 10.341 ), ( 0.1, 17.275 ), ( 0.05, 19.675 ), ( 0.025, 21.92 ), ( 0.01, 24.725 ), ( 0.005, 26.757 ) ] )
                , ( 12, Dict.fromList [ ( 0.995, 3.0738 ), ( 0.99, 3.5706 ), ( 0.975, 4.4038 ), ( 0.95, 5.226 ), ( 0.9, 6.3038 ), ( 0.5, 11.34 ), ( 0.1, 18.549 ), ( 0.05, 21.026 ), ( 0.025, 23.337 ), ( 0.01, 26.217 ), ( 0.005, 28.3 ) ] )
                , ( 13, Dict.fromList [ ( 0.995, 3.565 ), ( 0.99, 4.1069 ), ( 0.975, 5.0088 ), ( 0.95, 5.8919 ), ( 0.9, 7.0415 ), ( 0.5, 12.34 ), ( 0.1, 19.812 ), ( 0.05, 22.362 ), ( 0.025, 24.736 ), ( 0.01, 27.688 ), ( 0.005, 29.819 ) ] )
                , ( 14, Dict.fromList [ ( 0.995, 4.0747 ), ( 0.99, 4.6604 ), ( 0.975, 5.6287 ), ( 0.95, 6.5706 ), ( 0.9, 7.7895 ), ( 0.5, 13.339 ), ( 0.1, 21.064 ), ( 0.05, 23.685 ), ( 0.025, 26.119 ), ( 0.01, 29.141 ), ( 0.005, 31.319 ) ] )
                , ( 15, Dict.fromList [ ( 0.995, 4.6009 ), ( 0.99, 5.2293 ), ( 0.975, 6.2621 ), ( 0.95, 7.2609 ), ( 0.9, 8.5468 ), ( 0.5, 14.339 ), ( 0.1, 22.307 ), ( 0.05, 24.996 ), ( 0.025, 27.488 ), ( 0.01, 30.578 ), ( 0.005, 32.801 ) ] )
                , ( 16, Dict.fromList [ ( 0.995, 5.1422 ), ( 0.99, 5.8122 ), ( 0.975, 6.9077 ), ( 0.95, 7.9616 ), ( 0.9, 9.3122 ), ( 0.5, 15.338 ), ( 0.1, 23.542 ), ( 0.05, 26.296 ), ( 0.025, 28.845 ), ( 0.01, 32.0 ), ( 0.005, 34.267 ) ] )
                , ( 17, Dict.fromList [ ( 0.995, 5.6972 ), ( 0.99, 6.4078 ), ( 0.975, 7.5642 ), ( 0.95, 8.6718 ), ( 0.9, 10.085 ), ( 0.5, 16.338 ), ( 0.1, 24.769 ), ( 0.05, 27.587 ), ( 0.025, 30.191 ), ( 0.01, 33.409 ), ( 0.005, 35.718 ) ] )
                , ( 18, Dict.fromList [ ( 0.995, 6.2648 ), ( 0.99, 7.0149 ), ( 0.975, 8.2307 ), ( 0.95, 9.3905 ), ( 0.9, 10.865 ), ( 0.5, 17.338 ), ( 0.1, 25.989 ), ( 0.05, 28.869 ), ( 0.025, 31.526 ), ( 0.01, 34.805 ), ( 0.005, 37.156 ) ] )
                , ( 19, Dict.fromList [ ( 0.995, 6.844 ), ( 0.99, 7.6327 ), ( 0.975, 8.9065 ), ( 0.95, 10.117 ), ( 0.9, 11.651 ), ( 0.5, 18.338 ), ( 0.1, 27.204 ), ( 0.05, 30.144 ), ( 0.025, 32.852 ), ( 0.01, 36.191 ), ( 0.005, 38.582 ) ] )
                , ( 20, Dict.fromList [ ( 0.995, 7.4338 ), ( 0.99, 8.2604 ), ( 0.975, 9.5908 ), ( 0.95, 10.851 ), ( 0.9, 12.443 ), ( 0.5, 19.337 ), ( 0.1, 28.412 ), ( 0.05, 31.41 ), ( 0.025, 34.17 ), ( 0.01, 37.566 ), ( 0.005, 39.997 ) ] )
                , ( 21, Dict.fromList [ ( 0.995, 8.0337 ), ( 0.99, 8.8972 ), ( 0.975, 10.283 ), ( 0.95, 11.591 ), ( 0.9, 13.24 ), ( 0.5, 20.337 ), ( 0.1, 29.615 ), ( 0.05, 32.671 ), ( 0.025, 35.479 ), ( 0.01, 38.932 ), ( 0.005, 41.401 ) ] )
                , ( 22, Dict.fromList [ ( 0.995, 8.6427 ), ( 0.99, 9.5425 ), ( 0.975, 10.982 ), ( 0.95, 12.338 ), ( 0.9, 14.041 ), ( 0.5, 21.337 ), ( 0.1, 30.813 ), ( 0.05, 33.924 ), ( 0.025, 36.781 ), ( 0.01, 40.289 ), ( 0.005, 42.796 ) ] )
                , ( 23, Dict.fromList [ ( 0.995, 9.2604 ), ( 0.99, 10.196 ), ( 0.975, 11.689 ), ( 0.95, 13.091 ), ( 0.9, 14.848 ), ( 0.5, 22.337 ), ( 0.1, 32.007 ), ( 0.05, 35.172 ), ( 0.025, 38.076 ), ( 0.01, 41.638 ), ( 0.005, 44.181 ) ] )
                , ( 24, Dict.fromList [ ( 0.995, 9.8862 ), ( 0.99, 10.856 ), ( 0.975, 12.401 ), ( 0.95, 13.848 ), ( 0.9, 15.659 ), ( 0.5, 23.337 ), ( 0.1, 33.196 ), ( 0.05, 36.415 ), ( 0.025, 39.364 ), ( 0.01, 42.98 ), ( 0.005, 45.559 ) ] )
                , ( 25, Dict.fromList [ ( 0.995, 10.52 ), ( 0.99, 11.524 ), ( 0.975, 13.12 ), ( 0.95, 14.611 ), ( 0.9, 16.473 ), ( 0.5, 24.337 ), ( 0.1, 34.382 ), ( 0.05, 37.652 ), ( 0.025, 40.646 ), ( 0.01, 44.314 ), ( 0.005, 46.928 ) ] )
                , ( 26, Dict.fromList [ ( 0.995, 11.16 ), ( 0.99, 12.198 ), ( 0.975, 13.844 ), ( 0.95, 15.379 ), ( 0.9, 17.292 ), ( 0.5, 25.336 ), ( 0.1, 35.563 ), ( 0.05, 38.885 ), ( 0.025, 41.923 ), ( 0.01, 45.642 ), ( 0.005, 48.29 ) ] )
                , ( 27, Dict.fromList [ ( 0.995, 11.808 ), ( 0.99, 12.879 ), ( 0.975, 14.573 ), ( 0.95, 16.151 ), ( 0.9, 18.114 ), ( 0.5, 26.336 ), ( 0.1, 36.741 ), ( 0.05, 40.113 ), ( 0.025, 43.195 ), ( 0.01, 46.963 ), ( 0.005, 49.645 ) ] )
                , ( 28, Dict.fromList [ ( 0.995, 12.461 ), ( 0.99, 13.565 ), ( 0.975, 15.308 ), ( 0.95, 16.928 ), ( 0.9, 18.939 ), ( 0.5, 27.336 ), ( 0.1, 37.916 ), ( 0.05, 41.337 ), ( 0.025, 44.461 ), ( 0.01, 48.278 ), ( 0.005, 50.993 ) ] )
                , ( 29, Dict.fromList [ ( 0.995, 13.121 ), ( 0.99, 14.256 ), ( 0.975, 16.047 ), ( 0.95, 17.708 ), ( 0.9, 19.768 ), ( 0.5, 28.336 ), ( 0.1, 39.087 ), ( 0.05, 42.557 ), ( 0.025, 45.722 ), ( 0.01, 49.588 ), ( 0.005, 52.336 ) ] )
                , ( 30, Dict.fromList [ ( 0.995, 13.787 ), ( 0.99, 14.953 ), ( 0.975, 16.791 ), ( 0.95, 18.493 ), ( 0.9, 20.599 ), ( 0.5, 29.336 ), ( 0.1, 40.256 ), ( 0.05, 43.773 ), ( 0.025, 46.979 ), ( 0.01, 50.892 ), ( 0.005, 53.672 ) ] )
                , ( 40, Dict.fromList [ ( 0.995, 20.707 ), ( 0.99, 22.164 ), ( 0.975, 24.433 ), ( 0.95, 26.509 ), ( 0.9, 29.051 ), ( 0.5, 39.335 ), ( 0.1, 51.805 ), ( 0.05, 55.758 ), ( 0.025, 59.342 ), ( 0.01, 63.691 ), ( 0.005, 66.766 ) ] )
                , ( 50, Dict.fromList [ ( 0.995, 27.991 ), ( 0.99, 29.707 ), ( 0.975, 32.357 ), ( 0.95, 34.764 ), ( 0.9, 37.689 ), ( 0.5, 49.335 ), ( 0.1, 63.167 ), ( 0.05, 67.505 ), ( 0.025, 71.42 ), ( 0.01, 76.154 ), ( 0.005, 79.49 ) ] )
                , ( 60, Dict.fromList [ ( 0.995, 35.534 ), ( 0.99, 37.485 ), ( 0.975, 40.482 ), ( 0.95, 43.188 ), ( 0.9, 46.459 ), ( 0.5, 59.335 ), ( 0.1, 74.397 ), ( 0.05, 79.082 ), ( 0.025, 83.298 ), ( 0.01, 88.379 ), ( 0.005, 91.952 ) ] )
                , ( 70, Dict.fromList [ ( 0.995, 43.275 ), ( 0.99, 45.442 ), ( 0.975, 48.758 ), ( 0.95, 51.739 ), ( 0.9, 55.329 ), ( 0.5, 69.334 ), ( 0.1, 85.527 ), ( 0.05, 90.531 ), ( 0.025, 95.023 ), ( 0.01, 100.43 ), ( 0.005, 104.21 ) ] )
                , ( 80, Dict.fromList [ ( 0.995, 51.172 ), ( 0.99, 53.54 ), ( 0.975, 57.153 ), ( 0.95, 60.391 ), ( 0.9, 64.278 ), ( 0.5, 79.334 ), ( 0.1, 96.578 ), ( 0.05, 101.88 ), ( 0.025, 106.63 ), ( 0.01, 112.33 ), ( 0.005, 116.32 ) ] )
                , ( 90, Dict.fromList [ ( 0.995, 59.196 ), ( 0.99, 61.754 ), ( 0.975, 65.647 ), ( 0.95, 69.126 ), ( 0.9, 73.291 ), ( 0.5, 89.334 ), ( 0.1, 107.57 ), ( 0.05, 113.15 ), ( 0.025, 118.14 ), ( 0.01, 124.12 ), ( 0.005, 128.3 ) ] )
                , ( 100, Dict.fromList [ ( 0.995, 67.328 ), ( 0.99, 70.065 ), ( 0.975, 74.222 ), ( 0.95, 77.929 ), ( 0.9, 82.358 ), ( 0.5, 99.334 ), ( 0.1, 118.5 ), ( 0.05, 124.34 ), ( 0.025, 129.56 ), ( 0.01, 135.81 ), ( 0.005, 140.17 ) ] )
                ]
    in
        Maybe.withDefault (Dict.fromList [ ( 0.995, 67.328 ), ( 0.99, 70.065 ), ( 0.975, 74.222 ), ( 0.95, 77.929 ), ( 0.9, 82.358 ), ( 0.5, 99.334 ), ( 0.1, 118.5 ), ( 0.05, 124.34 ), ( 0.025, 129.56 ), ( 0.01, 135.81 ), ( 0.005, 140.17 ) ] ) (Dict.get n chiSquareDistribution)


{-| chiGet
-}
chiGet:Int -> Float -> Float
chiGet n cc =
    let
        pickUp = pickUpChi n
    in
        Maybe.withDefault 1 (Dict.get cc pickUp)


-- TODO: これに限らず値を取り出すのではなくちゃんと計算して求める関数にしていきたい。
{-| pickUpSignificance
  Int と Float を受け取って　有意水準の値を返す。

  pickUpSignificance 1 0.05
  OUT 0.996917
-}
pickUpSignificance:Int -> Basics.Float -> Basics.Float
pickUpSignificance key v =
    let
        table = Dict.fromList [
            (1, (Dict.fromList [(0.05, 0.996917),(0.01, 0.999877)])),
            (2, (Dict.fromList [(0.05, 0.950000),(0.01, 0.990000)])),
            (3, (Dict.fromList [(0.05, 0.878339),(0.01, 0.958735)])),
            (4, (Dict.fromList [(0.05, 0.811401),(0.01, 0.917200)])),
            (5, (Dict.fromList [(0.05, 0.754492),(0.01, 0.874526)])),
            (6, (Dict.fromList [(0.05, 0.706734),(0.01, 0.834342)])),
            (7, (Dict.fromList [(0.05, 0.666384),(0.01, 0.797681)])),
            (8, (Dict.fromList [(0.05, 0.631897),(0.01, 0.764592)])),
            (9, (Dict.fromList [(0.05, 0.602069),(0.01, 0.734786)])),
            (10, (Dict.fromList [(0.05, 0.575983),(0.01, 0.707888)])),
            (11, (Dict.fromList [(0.05, 0.552943),(0.01, 0.683528)])),
            (12, (Dict.fromList [(0.05, 0.532413),(0.01, 0.661376)])),
            (13, (Dict.fromList [(0.05, 0.513977),(0.01, 0.641145)])),
            (14, (Dict.fromList [(0.05, 0.497309),(0.01, 0.622591)])),
            (15, (Dict.fromList [(0.05, 0.482146),(0.01, 0.605506)])),
            (16, (Dict.fromList [(0.05, 0.468277),(0.01, 0.589714)])),
            (17, (Dict.fromList [(0.05, 0.455531),(0.01, 0.575067)])),
            (18, (Dict.fromList [(0.05, 0.443763),(0.01, 0.561435)])),
            (19, (Dict.fromList [(0.05, 0.432858),(0.01, 0.548711)])),
            (20, (Dict.fromList [(0.05, 0.422714),(0.01, 0.536800)])),
            (21, (Dict.fromList [(0.05, 0.413247),(0.01, 0.525620)])),
            (22, (Dict.fromList [(0.05, 0.404386),(0.01, 0.515101)])),
            (23, (Dict.fromList [(0.05, 0.396070),(0.01, 0.505182)])),
            (24, (Dict.fromList [(0.05, 0.388244),(0.01, 0.495808)])),
            (25, (Dict.fromList [(0.05, 0.380863),(0.01, 0.486932)])),
            (26, (Dict.fromList [(0.05, 0.373886),(0.01, 0.478511)])),
            (27, (Dict.fromList [(0.05, 0.367278),(0.01, 0.470509)])),
            (28, (Dict.fromList [(0.05, 0.361007),(0.01, 0.462892)]))
            ]
        tList = Maybe.withDefault (Dict.fromList [(0.05, 0.361007),(0.01, 0.462892)]) (Dict.get key table)
        in
            Maybe.withDefault 0 (Dict.get v tList)
