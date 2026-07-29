package com.sinanspd

import spire.math.Complex


private[sinanspd] object RotationMath {
  private val one = Complex(1d, 0d)

  private def angle(thetaDenominator: Int): Double = {
    require(thetaDenominator != 0, "A rotation denominator cannot be zero")
    math.Pi / thetaDenominator.toDouble
  }

  private def unitPhase(radians: Double): Complex[Double] =
    Complex(math.cos(radians), math.sin(radians))

  def multiply(
      value: Complex[Double],
      coefficient: Complex[Double]
  ): Complex[Double] =
    Complex(
      value.real * coefficient.real - value.imag * coefficient.imag,
      value.real * coefficient.imag + value.imag * coefficient.real
    )

  /** Diagonal coefficient of P(pi / d) for the selected basis bit. */
  def phaseCoefficient(
      thetaDenominator: Int,
      targetIsOne: Boolean
  ): Complex[Double] = {
    val radians = angle(thetaDenominator)
    if (targetIsOne) unitPhase(radians) else one
  }

  /** Diagonal coefficient of Rz(pi / d) for the selected basis bit. */
  def rzCoefficient(
      thetaDenominator: Int,
      targetIsOne: Boolean
  ): Complex[Double] = {
    val halfAngle = angle(thetaDenominator) / 2d
    unitPhase(if (targetIsOne) halfAngle else -halfAngle)
  }

  /** Coefficient of the project's historical controlled-rotation gate. */
  def controlledRotateCoefficient(
      thetaDenominator: Int,
      controlIsOne: Boolean,
      targetIsOne: Boolean
  ): Complex[Double] = {
    val halfAngle = angle(thetaDenominator) / 2d
    if (controlIsOne && targetIsOne) {
      unitPhase(halfAngle)
    } else {
      one
    }
  }

  def applyPhase(
      value: Complex[Double],
      thetaDenominator: Int,
      targetIsOne: Boolean
  ): Complex[Double] =
    multiply(value, phaseCoefficient(thetaDenominator, targetIsOne))

  def applyRz(
      value: Complex[Double],
      thetaDenominator: Int,
      targetIsOne: Boolean
  ): Complex[Double] =
    multiply(value, rzCoefficient(thetaDenominator, targetIsOne))

  def applyControlledRotate(
      value: Complex[Double],
      thetaDenominator: Int,
      controlIsOne: Boolean,
      targetIsOne: Boolean
  ): Complex[Double] =
    multiply(
      value,
      controlledRotateCoefficient(thetaDenominator, controlIsOne, targetIsOne)
    )
}
