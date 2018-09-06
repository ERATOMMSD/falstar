package falstar.linear

object Integrator {
  def euler(flow: (Double, Vector, Vector) => Vector, t: Double, dt: Double, x: Vector, u: Vector) = {
    x + flow(t, x, u) * dt
  }

  // 4th order Runge/Kutta integrator
  def rk4(flow: (Double, Vector, Vector) => Vector, t: Double, dt: Double, x: Vector, u: Vector): (Double, Vector) = {
    val h = dt
    val h2 = h / 2
    val h6 = h / 6

    val k1 = flow(t, x, u)
    val k2 = flow(t + h2, x + h2 * k1, u)
    val k3 = flow(t + h2, x + h2 * k2, u)
    val k4 = flow(t + h, x + h * k3, u)
    val dx = h6 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

    (t + dt, x + dx)
  }

  // 4/5th order Dormand Prince adaptive integrator
  def dp45(flow: (Double, Vector, Vector) => Vector, t0: Double, dt: Double, x0: Vector, u: Vector, hmin: Double, hmax: Double, step: (Double, Vector) => Any = null, tolerance: Double = 1e-5, maxiter: Int = 1000): (Double, Vector) = {
    assert(0 < dt, "invalid dt = " + dt)
    assert(0 < hmin && hmin <= hmax, "invalid hmin = " + hmin + ", hmax = " + hmax)
    assert(0 < tolerance, "invalid tolerance = " + tolerance)

    val a21 = (1.0 / 5.0)
    val a31 = (3.0 / 40.0)
    val a32 = (9.0 / 40.0)
    val a41 = (44.0 / 45.0)
    val a42 = (-56.0 / 15.0)
    val a43 = (32.0 / 9.0)
    val a51 = (19372.0 / 6561.0)
    val a52 = (-25360.0 / 2187.0)
    val a53 = (64448.0 / 6561.0)
    val a54 = (-212.0 / 729.0)
    val a61 = (9017.0 / 3168.0)
    val a62 = (-355.0 / 33.0)
    val a63 = (46732.0 / 5247.0)
    val a64 = (49.0 / 176.0)
    val a65 = (-5103.0 / 18656.0)
    val a71 = (35.0 / 384.0)
    val a72 = (0.0)
    val a73 = (500.0 / 1113.0)
    val a74 = (125.0 / 192.0)
    val a75 = (-2187.0 / 6784.0)
    val a76 = (11.0 / 84.0)

    val c2 = (1.0 / 5.0)
    val c3 = (3.0 / 10.0)
    val c4 = (4.0 / 5.0)
    val c5 = (8.0 / 9.0)
    val c6 = (1.0)
    val c7 = (1.0)

    val b1 = (35.0 / 384.0)
    val b2 = (0.0)
    val b3 = (500.0 / 1113.0)
    val b4 = (125.0 / 192.0)
    val b5 = (-2187.0 / 6784.0)
    val b6 = (11.0 / 84.0)
    val b7 = (0.0)

    val b1p = (5179.0 / 57600.0)
    val b2p = (0.0)
    val b3p = (7571.0 / 16695.0)
    val b4p = (393.0 / 640.0)
    val b5p = (-92097.0 / 339200.0)
    val b6p = (187.0 / 2100.0)
    val b7p = (1.0 / 40.0)

    var t = t0
    val t1 = t0 + dt
    var x = x0
    var h = hmax

    for (i <- 0 until maxiter) {
      // println("t = " + t)
      // println("x = " + x)
      // println("h = " + h)

      val k1 = flow(t, x, u)
      val k2 = flow(t + c2 * h, x + h * (a21 * k1), u)
      val k3 = flow(t + c3 * h, x + h * (a31 * k1 + a32 * k2), u)
      val k4 = flow(t + c4 * h, x + h * (a41 * k1 + a42 * k2 + a43 * k3), u)
      val k5 = flow(t + c5 * h, x + h * (a51 * k1 + a52 * k2 + a53 * k3 + a54 * k4), u)
      val k6 = flow(t + h, x + h * (a61 * k1 + a62 * k2 + a63 * k3 + a64 * k4 + a65 * k5), u)
      val k7 = flow(t + h, x + h * (a71 * k1 + a72 * k2 + a73 * k3 + a74 * k4 + a75 * k5 + a76 * k6), u)

      // println("k1 = " + k1)
      // println("k2 = " + k2)
      // println("k3 = " + k3)
      // println("k4 = " + k4)
      // println("k5 = " + k5)
      // println("k6 = " + k6)
      // println("k7 = " + k7)

      val dist = ((b1 - b1p) * k1 + (b3 - b3p) * k3 + (b4 - b4p) * k4 + (b5 - b5p) * k5 + (b6 - b6p) * k6 + (b7 - b7p) * k7)
      val error = dist.norm
      val delta = 0.84 * Math.pow(tolerance / error, (1.0 / 5.0))

      // println("error = " + error)
      if (h <= hmin || error < tolerance) {
        t += h
        x += h * (b1 * k1 + b3 * k3 + b4 * k4 + b5 * k5 + b6 * k6)

        if (step != null) {
          step(t, x)
        }
      }

      if (delta <= 0.1)
        h = h * 0.1
      else if (delta >= 4.0)
        h = h * 4.0
      else
        h = delta * h

      if (t >= t1)
        return (t, x)

      if (h > hmax)
        h = hmax

      if (h < hmin)
        h = hmin

      if (t + h > t1) {
        h = t1 - t
      } else {
        assert(h >= hmin, "too small step size " + h + " at time " + t + " in state " + x)
      }
    }

    assert(false, "too many iterations")

    ???
  }
}