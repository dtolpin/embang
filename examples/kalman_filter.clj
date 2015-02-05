(ns kalman-filter
  (:require [clojure.core.matrix
             :refer [identity-matrix mmul]
             :rename {identity-matrix eye}])
  (:use [embang runtime emit]))
  
;; generative model
;;
;; x[t] ~ mvn(A . x[t-1], W)
;; y[t] ~ mvn(H . x[t], Q)

(declare H)
(declare data)

(declare predict-angles x)

(with-primitive-procedures [eye mmul]
  (defanglican kalman-filter
    ;; number of time points
    [assume T 50]
    ;; dimensionality of observations
    [assume D 16]
    ;; transition matrix
    ;; (assumed to be a 2-D rotation, periodic over 100 time points)
    ;; [assume omega (* (/ (* 2. Math/PI) T) 
    ;;                  (sample (poisson 6.)))]
    [assume omega (* (sample (gamma 10. 2.5))
                     (/ Math/PI T))]
    [assume A (vector (vector (cos omega) (- (sin omega)))
                      (vector (sin omega) (cos omega)))]
    ;; transition covariance
    ;; (assumed to be diagonal with uniform error rate)
    [assume w (sample (gamma 10. 100.))]
    [assume W (mmul (eye 2) w)]
    ;; observation matrix H
    ;; (assumed to be somewhat sparse)
    ;; observation covariance 
    ;; (assumed to be diagonal with uniform error rate)
    [assume q 1.0e-02]
    [assume Q (mmul (eye D) q)]
    ;; latent coordinate sequence
    [assume x (mem (lambda (t)
                     (if (< t 1)
                       (vector 1. 0.)
                       (sample (mvn (mmul A (x (dec t))) W)))))]
    ;; observed data
    (reduce (lambda (_ record)
              [observe (mvn (mmul H (x (first record))) Q) (second record)])
            () data)

    ;; predict parameters
    [predict omega]
    [predict w]
    (predict-angles x)))

(defun angle (x) (atan (/ (second x) (first x))))

(defun predict-angles (x)
    [predict (angle (x 1))]
    [predict (angle (x 2))]
    [predict (angle (x 3))]
    [predict (angle (x 4))]
    [predict (angle (x 5))]
    [predict (angle (x 6))]
    [predict (angle (x 7))]
    [predict (angle (x 8))]
    [predict (angle (x 9))]
    [predict (angle (x 10))]
    [predict (angle (x 11))]
    [predict (angle (x 12))]
    [predict (angle (x 13))]
    [predict (angle (x 14))]
    [predict (angle (x 15))]
    [predict (angle (x 16))]
    [predict (angle (x 17))]
    [predict (angle (x 18))]
    [predict (angle (x 19))]
    [predict (angle (x 20))]
    [predict (angle (x 21))]
    [predict (angle (x 22))]
    [predict (angle (x 23))]
    [predict (angle (x 24))]
    [predict (angle (x 25))]
    [predict (angle (x 26))]
    [predict (angle (x 27))]
    [predict (angle (x 28))]
    [predict (angle (x 29))]
    [predict (angle (x 30))]
    [predict (angle (x 31))]
    [predict (angle (x 32))]
    [predict (angle (x 33))]
    [predict (angle (x 34))]
    [predict (angle (x 35))]
    [predict (angle (x 36))]
    [predict (angle (x 37))]
    [predict (angle (x 38))]
    [predict (angle (x 39))]
    [predict (angle (x 40))]
    [predict (angle (x 41))]
    [predict (angle (x 42))]
    [predict (angle (x 43))]
    [predict (angle (x 44))]
    [predict (angle (x 45))]
    [predict (angle (x 46))]
    [predict (angle (x 47))]
    [predict (angle (x 48))]
    [predict (angle (x 49))]
    [predict (angle (x 50))])


(def H [[1.7435e-01 2.5215e-03]
        [1.5101e-08 1.4628e-01]
        [1.1861e-03 5.9692e-02]
        [3.5349e-01 1.1220e-07]
        [1.8924e-02 2.2386e-04]
        [1.8625e-02 1.4326e-02]
        [5.8814e-22 7.9644e-04]
        [1.9282e-06 3.1970e-02]
        [4.1361e-02 3.1349e-03]
        [3.5481e-01 3.5090e-04]
        [4.0861e-03 8.5815e-02]
        [1.7040e-04 3.0770e-05]
        [9.1995e-05 1.5596e-03]
        [3.2624e-02 4.4134e-10]
        [6.6385e-17 4.7347e-02]
        [2.8195e-04 6.0595e-01]])

(def data
  [[1 [3.0852e-03 6.9439e-02 5.7780e-02 3.4676e-01 1.1497e-01 -3.9397e-02 -6.2434e-02 -1.0043e-01 -4.0935e-02 3.0100e-01 -1.3963e-02 -1.1565e-01 8.1338e-02 6.8996e-02 8.8767e-02 2.5206e-02]]
    [2 [7.3722e-02 1.0727e-01 2.5952e-02 3.9253e-01 -2.0681e-03 2.1119e-01 3.0483e-03 8.3030e-02 3.2700e-02 1.0143e-01 6.9931e-02 1.5356e-02 -1.5635e-01 -4.0049e-02 1.9222e-03 -2.6356e-02]]
    [3 [1.6686e-01 1.2143e-01 -4.9040e-02 2.7357e-01 -4.6660e-02 5.4066e-02 9.3124e-02 -1.5183e-01 -8.8246e-02 7.5608e-02 3.7071e-02 -1.9280e-02 6.7195e-02 -1.5425e-01 -2.1773e-01 -1.9935e-02]]
    [4 [4.6086e-02 4.7700e-02 3.6401e-02 8.6298e-02 -3.3745e-02 1.2076e-02 -1.2208e-01 -1.6662e-01 1.8199e-01 2.4655e-01 8.3638e-02 5.5599e-02 1.3137e-01 -3.0493e-02 -1.0193e-02 3.0522e-01]]
    [5 [7.0310e-02 6.4494e-02 7.1797e-02 -4.1467e-03 -2.5155e-02 1.2846e-02 6.8739e-02 9.1927e-02 -6.8837e-02 1.0728e-01 4.6146e-02 -1.6773e-01 4.4092e-02 -1.5314e-01 5.2755e-03 6.4245e-01]]
    [6 [2.5436e-02 1.0750e-01 -1.3541e-01 3.3867e-01 -7.0459e-02 1.6263e-01 -1.0407e-02 1.4878e-01 2.1005e-01 2.0722e-01 2.9390e-01 -6.7851e-02 7.4042e-02 -7.4709e-02 -8.2813e-02 4.4669e-01]]
    [7 [4.1012e-02 3.0485e-02 -2.1571e-02 -4.5355e-02 -9.2957e-02 1.8353e-01 1.2145e-01 1.4586e-01 -9.6536e-02 -7.1386e-02 1.0943e-02 -2.8929e-02 -7.2512e-02 -7.3597e-02 1.1180e-01 3.8402e-01]]
    [8 [-7.6979e-02 -1.4035e-02 1.8177e-01 7.1690e-02 1.1543e-01 -4.1921e-02 3.1629e-02 -2.9916e-02 -1.0870e-01 -3.5560e-02 -1.8922e-01 -1.5249e-02 -7.3823e-02 -8.9728e-03 1.8693e-01 5.8833e-01]]
    [9 [-3.3870e-02 3.2908e-01 -8.9162e-03 1.2673e-02 1.2409e-01 -9.8379e-03 -4.3556e-02 8.7502e-02 -9.7749e-02 -2.2984e-01 1.7115e-01 1.0230e-01 1.1271e-01 4.6955e-03 2.5215e-02 7.9253e-01]]
    [10 [-2.0393e-01 2.4523e-01 -1.6856e-02 -4.4569e-01 -1.1698e-01 2.2954e-02 6.4876e-04 2.8723e-02 -1.5841e-01 -2.0721e-01 1.5128e-01 -6.4320e-02 1.0505e-01 -1.2709e-01 7.5798e-02 3.9697e-01]]
    [11 [6.1252e-02 -3.2806e-02 1.0764e-02 -2.4819e-01 3.3577e-02 -1.2230e-01 -2.5479e-02 -4.1803e-02 -1.4328e-01 -1.1504e-01 -6.4965e-02 8.0360e-02 -1.4917e-02 1.8617e-01 -6.4478e-02 2.1220e-01]]
    [12 [-1.4325e-01 1.3327e-01 7.5271e-02 -2.2750e-01 -2.6703e-01 1.3538e-02 8.1332e-03 -1.0428e-02 -1.5238e-01 -3.0145e-01 3.0586e-02 -4.9440e-02 -1.0655e-02 -5.4672e-02 3.3044e-02 2.7575e-01]]
    [13 [-3.2881e-01 1.3744e-01 -7.7660e-02 -9.9467e-02 -5.1046e-02 2.9771e-02 -1.2289e-01 6.2204e-02 -9.6209e-03 -1.6084e-01 1.4706e-01 1.3497e-01 -3.8453e-02 7.3879e-02 1.8227e-01 -4.4537e-02]]
    [14 [-5.3775e-02 1.8890e-02 1.3170e-01 -4.1254e-01 -1.0146e-01 -5.7702e-02 1.0472e-01 9.2392e-02 -3.3188e-02 -1.4620e-01 2.6688e-02 -4.3569e-02 3.5827e-02 -5.5998e-02 2.1287e-01 1.4077e-01]]
    [15 [1.1281e-01 9.0425e-03 -8.4429e-02 -1.6365e-01 1.1982e-01 -1.4906e-01 -1.4289e-02 -6.6673e-02 -1.2131e-02 -3.4665e-01 -1.0642e-01 2.6131e-01 -6.8739e-02 -9.3291e-02 7.0988e-02 -2.1726e-02]]
    [16 [2.2429e-03 -2.1331e-02 -6.6929e-02 1.8204e-02 2.4813e-03 1.1730e-01 6.4028e-02 -4.0898e-02 -1.9798e-02 -2.4697e-01 1.6388e-02 -1.1586e-01 1.1106e-01 5.1147e-02 1.4527e-01 3.4336e-01]]
    [17 [-1.6724e-01 1.3420e-01 4.9780e-02 -1.8703e-01 9.0790e-02 -1.2283e-01 -2.6274e-02 1.3317e-01 -1.8407e-01 -3.0198e-01 1.2865e-01 -1.5769e-02 -2.4484e-01 -1.2172e-01 1.6370e-01 9.5147e-02]]
    [18 [-1.3853e-01 -1.3268e-02 -1.4120e-01 3.3701e-02 4.6589e-02 -3.1064e-02 -9.1824e-02 -1.0281e-01 1.9923e-01 -1.7697e-01 1.6571e-01 -1.5825e-01 -7.7739e-02 3.8347e-02 2.8676e-03 2.9357e-01]]
    [19 [-1.3672e-01 1.9378e-02 -6.7887e-02 -6.9882e-02 -2.5986e-02 3.4188e-02 9.4191e-02 5.4705e-02 1.1956e-02 -2.0521e-01 1.4290e-01 5.6514e-02 4.5663e-02 7.0230e-04 -6.3477e-02 -3.0724e-02]]
    [20 [-2.2180e-02 -7.1622e-02 -9.3586e-02 6.1501e-02 -1.2850e-01 5.7039e-02 -1.5850e-01 -6.4525e-02 -3.2886e-02 9.0853e-02 2.2086e-02 2.3494e-02 2.7660e-02 1.3705e-01 1.4179e-01 2.5121e-01]]
    [21 [1.3094e-02 1.0287e-02 -4.9793e-02 1.0058e-02 -3.0392e-02 -2.2695e-01 6.5242e-03 4.3709e-02 1.8243e-01 1.7398e-01 -3.9111e-02 7.6209e-02 1.2635e-02 5.9673e-02 -1.1405e-01 -2.5019e-01]]
    [22 [-1.7717e-01 4.3338e-02 -9.3853e-02 1.3672e-02 4.2703e-02 -5.9138e-02 -3.6152e-02 5.4837e-02 -1.0164e-01 1.7495e-01 -1.7992e-01 -9.1148e-02 6.5767e-02 -1.5466e-01 4.3451e-02 -4.7979e-02]]
    [23 [-5.1112e-02 4.3034e-02 5.6914e-02 1.7617e-02 6.0642e-03 -3.2326e-02 -7.2693e-02 1.7960e-02 -5.6602e-02 5.6809e-04 -9.2343e-02 -8.2839e-03 -1.9398e-01 1.3719e-01 1.5113e-01 5.1223e-02]]
    [24 [-2.4770e-02 -1.4971e-01 -8.1196e-02 -6.0515e-02 9.2617e-03 -4.7847e-02 -6.3824e-02 8.2798e-02 -6.9468e-02 2.4940e-02 9.6224e-02 -5.6966e-03 2.3045e-01 5.0626e-02 1.6109e-01 2.8182e-01]]
    [25 [-2.5595e-01 8.1226e-02 3.5181e-02 -2.9832e-01 -5.8840e-02 5.2923e-03 1.7093e-01 -9.3093e-02 -8.0789e-02 -2.3775e-01 1.3102e-01 5.0294e-02 -1.3863e-01 5.3026e-03 6.6305e-02 1.3039e-01]]
    [26 [-1.9515e-01 -2.8010e-01 6.9038e-02 -1.3002e-01 1.2216e-01 -1.7475e-01 1.0903e-01 2.0933e-01 -4.0472e-02 -2.3773e-01 7.4684e-02 -6.5401e-02 -6.5108e-02 3.9737e-02 4.0580e-02 8.0193e-02]]
    [27 [-9.5533e-02 -5.3419e-03 -1.1223e-01 -9.8702e-02 -4.1526e-02 -4.9459e-02 1.2894e-02 1.6458e-02 3.8896e-02 -3.5941e-02 2.7615e-02 -9.4086e-02 1.0026e-01 -5.7029e-02 -8.7127e-02 2.0374e-01]]
    [28 [-1.6404e-01 2.6490e-01 -6.8077e-02 -1.0155e-01 5.4225e-02 1.4427e-01 1.9424e-01 6.1935e-02 3.3472e-02 1.2783e-02 -1.0026e-01 9.2933e-03 -5.3585e-02 -1.5785e-01 3.0067e-02 9.9104e-02]]
    [29 [-7.6509e-02 8.2532e-02 -1.4114e-01 -2.3114e-01 1.2382e-02 -6.7685e-02 -2.3990e-02 2.8486e-02 -1.2800e-02 -1.1619e-01 -1.6830e-02 -5.3585e-02 -3.0477e-02 -3.6533e-02 -1.0114e-01 1.6262e-01]]
    [30 [-9.5042e-02 -7.0990e-02 4.9696e-02 -6.7652e-02 3.6910e-02 1.3618e-02 -2.6283e-02 -3.9967e-02 6.9895e-02 -1.0675e-01 -4.5653e-02 8.9653e-02 -9.6902e-02 1.7900e-01 2.3169e-02 7.2133e-02]]
    [31 [-2.1516e-01 -3.5187e-02 -1.4916e-02 -1.1485e-01 -1.1452e-01 -9.7489e-02 7.5638e-02 -3.4546e-02 -8.1632e-02 -3.3215e-01 7.1153e-02 6.6982e-03 -9.2395e-02 6.0414e-02 2.3131e-02 3.5924e-01]]
    [32 [-1.7179e-01 1.4204e-01 2.2946e-02 -7.9981e-02 -8.6598e-03 1.2490e-01 -4.3039e-02 1.5076e-01 7.4673e-02 -9.7452e-02 6.0665e-02 5.4374e-02 -9.3687e-02 -1.3334e-01 1.1098e-01 1.4878e-01]]
    [33 [-2.2668e-01 1.1119e-01 -1.4844e-01 -2.1194e-01 7.8308e-02 -1.5788e-01 -3.9786e-02 1.4867e-01 -1.2024e-01 -8.2758e-02 1.4087e-01 -1.4921e-01 -2.5041e-03 1.1294e-01 7.7593e-03 6.4565e-02]]
    [34 [-1.6400e-01 -2.9986e-02 5.8737e-02 -5.5482e-01 2.7098e-02 -1.5681e-01 2.0314e-01 -4.4200e-02 -8.4894e-02 -7.4621e-01 1.0373e-02 4.1664e-02 2.0526e-01 -9.1737e-02 -1.6004e-01 1.2798e-01]]
    [35 [-3.4455e-01 -3.6315e-02 -8.3051e-02 -6.4273e-01 8.8851e-02 -7.5578e-02 8.6139e-02 -3.2643e-02 -5.6309e-02 -7.1901e-01 7.0437e-02 -2.8736e-02 3.2358e-02 -1.4561e-01 -1.2689e-01 3.4479e-02]]
    [36 [-2.5002e-01 4.3314e-02 -9.1532e-02 -5.1009e-01 4.1301e-02 1.8752e-02 1.7914e-02 6.1599e-03 -8.7421e-02 -5.0990e-01 7.9685e-02 -8.2230e-02 -2.6869e-02 5.0463e-03 -6.8720e-03 -2.8315e-01]]
    [37 [-3.2607e-01 -6.8207e-02 -1.4212e-01 -1.1021e-01 -1.0228e-01 1.7597e-02 -8.7973e-03 -1.7528e-01 1.1741e-01 -3.1822e-01 -7.3905e-03 -4.5673e-02 -8.8242e-02 8.3684e-02 -4.5975e-02 -7.0608e-01]]
    [38 [1.5682e-02 -2.5743e-01 -1.2411e-01 -3.8174e-01 3.1187e-02 -1.2095e-01 -5.1573e-02 -1.0591e-03 1.2914e-01 -1.9885e-01 -3.1433e-01 -1.8009e-02 -8.5469e-02 1.0506e-01 -1.1824e-01 -9.7766e-01]]
    [39 [-1.5535e-02 -2.5552e-01 1.0704e-01 -2.4022e-01 -2.7013e-02 -6.5520e-02 -3.9168e-02 -1.7503e-01 2.7888e-02 -1.8541e-01 -8.3275e-02 -1.0386e-02 8.3554e-04 2.4546e-01 -1.4552e-01 -9.0785e-01]]
    [40 [9.6466e-02 -1.0050e-01 -3.7904e-02 -3.8679e-01 3.2902e-02 1.3389e-01 -9.8768e-02 1.4353e-02 5.0942e-02 -1.8820e-01 -2.2362e-01 4.7137e-02 1.6867e-01 -1.4583e-01 -7.9217e-02 -1.1279e+00]]
    [41 [-1.4857e-01 -3.0538e-01 -6.8272e-02 -1.4616e-01 -4.6398e-02 -1.3817e-01 1.7694e-02 -2.2040e-01 -8.5187e-02 -2.4004e-01 -2.6168e-01 -4.1455e-02 1.1510e-01 -5.8385e-02 8.0896e-02 -9.4823e-01]]
    [42 [-1.3397e-01 -2.0574e-01 -9.4020e-02 -2.9887e-01 -6.7954e-03 -2.2667e-01 5.7647e-02 -5.4982e-02 1.2776e-02 -4.0394e-01 -9.6187e-02 1.3733e-02 -5.3326e-02 -4.9160e-02 -5.3341e-02 -1.1531e+00]]
    [43 [-1.2896e-01 -2.7993e-01 -8.9286e-02 -4.5247e-02 -3.5834e-02 -1.8914e-01 -1.2963e-01 9.1803e-02 5.2219e-02 -9.6104e-02 -6.2130e-02 2.6113e-02 1.3211e-02 -6.3008e-02 -5.1405e-02 -9.9649e-01]]
    [44 [1.2152e-01 -3.1344e-01 -1.4374e-01 2.0950e-01 1.5594e-01 -3.3602e-03 2.5348e-02 -4.7665e-02 -2.4660e-01 -5.0181e-03 -1.6765e-02 5.5500e-02 -1.3120e-01 5.2414e-02 -2.4279e-01 -1.0112e+00]]
    [45 [5.1754e-02 -2.3741e-01 -2.4943e-03 2.6501e-01 -1.0179e-01 1.3578e-01 2.5618e-02 -1.2624e-01 -3.3134e-02 1.6006e-01 -3.0970e-03 -9.0994e-02 5.1062e-02 1.9603e-02 -1.7335e-01 -7.1639e-01]]
    [46 [7.8931e-02 -2.4737e-01 6.9330e-02 4.4789e-02 -4.5341e-02 8.5456e-02 2.1881e-01 -8.8983e-03 4.5026e-02 2.4118e-01 -1.8791e-01 -7.2386e-02 1.6681e-01 -1.3332e-02 8.0621e-02 -9.3342e-01]]
    [47 [-4.1482e-02 -3.9403e-01 -1.4498e-01 2.8361e-01 -8.1689e-02 -6.4441e-02 -5.2852e-02 1.0101e-01 -6.7175e-02 1.9923e-01 -2.9223e-01 3.8866e-02 1.5632e-01 1.0317e-01 -5.6487e-02 -1.0869e+00]]
    [48 [2.4965e-01 -1.5419e-01 -3.3920e-02 1.8036e-01 -4.5629e-02 -8.6515e-02 1.0623e-01 -1.1537e-01 2.1038e-01 3.6348e-01 -1.0922e-01 1.0703e-02 2.2265e-02 -1.1750e-01 -1.4582e-01 -1.0660e+00]]
    [49 [2.0180e-02 -4.0645e-01 -2.0926e-01 2.2857e-01 9.0316e-02 -6.6879e-03 6.0363e-02 -1.0821e-01 -2.7369e-03 3.0745e-01 -5.4711e-02 2.5546e-02 5.9082e-03 3.1657e-02 -1.4576e-01 -1.0088e+00]]
    [50 [1.9765e-01 -9.5476e-02 -8.8120e-02 3.0295e-01 -1.4055e-01 -1.3474e-01 -5.3639e-02 -1.5403e-01 -2.5956e-02 1.6712e-01 -1.6509e-01 3.3005e-02 -8.4615e-02 -3.1582e-02 -1.1673e-01 -6.8144e-01]]])
