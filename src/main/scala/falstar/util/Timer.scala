package falstar.util

class Timer {
  var _running = false

  var _start = 0L
  var _time = 0L
  var _laps = 0L

  def start() {
    assert(!_running)
    if (!_running) {
      _start = System.currentTimeMillis()
      _running = true
      _laps += 1
    }
  }

  def stop() = {
    assert(_running)
    _running = false
    val _end = System.currentTimeMillis()
    val delta = _end - _start
    _time += delta
    delta
  }

  def reset() {
    assert(!_running)
    _time = 0L
  }

  def during[A](f: => A): A = {
    try { start(); f }
    finally { stop() }
  }

  def time = {
    if (_running) {
      val delta = System.currentTimeMillis() - _start
      _time + delta
    } else {
      _time
    }
  }

  def milliseconds = {
    time
  }

  def seconds = {
    time / 1000
  }

  def laps = {
    _laps
  }
}