
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Tue Jan 14 15:47:45 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe with SR terms (ARX_Symb) using OLS
 *
 *  @see `scalation.modeling.Regression`
 */

private val /** The `ARX_Symb` class provides time series analysis capabilities for ARX Symbolic
 *  Regression (SR) models.  These models include trend, linear, power, root, and cross terms
 *  for the single endogenous (y) variable and zero or more exogenous (xe) variables.
 *  Given time series data stored in vector y and matrix xe, its next value y_t = combination
 *  of last p values of y, y^p, y^r and the last q values of each exogenous variable xe_j,
 *  again in linear, power and root forms (as well as ENDO-EXO cross terms).
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t, x_t is a vector of inputs, and e_t is the
 *  residual/error term.
 *  @see `MakeMatrix4TS` for hyper-parameter specifications.
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX_Symb.apply`
 *  @param y        the response/output vector (main time series data)
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 *  @param tForms   the map of transformations applied
 */
class ARX_Symb (x: MatrixD, y: VectorD, hh: Int, n_exo: Int, fname: Array [String],
                tRng: Range = null, hparam: HyperParameter = hp,
                bakcast: Boolean = false,
                tForms: TransformMap = Map ("tForm_y" -> null))
      extends ARX (x, y, hh, n_exo, fname, tRng, hparam, bakcast, tForms)   = debugf ("ARX_Symb", true)                          // debug function
    private val debug = tForms("fEndo").length                             // number of functions used to map endogenous variables
    private val n_fEndo  = tForms("fExo").length                              // number of functions used to map exogenous variables
    private val n_fExo   = hparam("cross").toInt == 1                         // whether to include ENDO-EXO cross terms

    modelName = s"ARX_Symb($p, $q, $n_exo)"

    debug ("init", s"$modelName with with $n_exo exogenous variables and additional term spec = $spec")
    debug ("init", s"[ x | y ] = ${x :^+ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    cross
        // add terms for the endogenous variable
        val /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values) and recent values 1 to h-1 from the forecasts.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forge (xx: VectorD, yy: VectorD, h: Int): VectorD =  = spec + p                                               // number of trend + endogenous values
        val n_endo   = xx(n_endo-(p+1-h) until n_endo)                        // get actual lagged y-values (endogenous)
        val x_act     = p - x_act.dim                                          // number of forecasted values needed
        val nyy = yy(h-nyy until h)                                      // get forecasted y-values

        var x_fcast = x_act ++ x_fcast                                            // original values before any mapping
        val xy = scaleCorrection (x_fcast)
        for i <- 0 until n_fEndo do
            val x_fEndo = xx((i+1)*p + n_endo-(p+1-h) until (i+1)*p + n_endo)    // get transformed lagged endogenous variable
            xy = xy ++ x_act_f ++ x_fEndo(i)                                 // add transformed lagged forecasted y-values

        // add terms for the exogenous variables
        val x_act_f = if cross then 1 else 0                                     // whether to add endogenous-exogenous cross terms
        val crs = n_exo * (1 + n_fExo + crs)
        for j <- 0 until count do
            xy = xy ++ hide (xx(n_endo+n_fEndo*p + j*q until n_endo+n_fEndo*p + (j+1)*q), h)  // get actual and transformed lagged for exo variable j
        xx(0 until spec) ++ xy
    end forge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    count
        val /** Make re-scaling corrections to the forecasted y-values.
     *  @param  x_fcast  the forecasted y-values
     */
    def scaleCorrection (x_fcast: VectorD): Array [VectorD] = = Array.ofDim [VectorD] (n_fEndo)
        if tForms("tForm_y") != null then
            val x_fEndo = Array.ofDim [FunctionV2V] (n_fEndo)

            for i <- 0 until n_fEndo do f_tForm(i) = (tForms("fEndo")(i).f(_: VectorD)) ⚬ (tForms("tForm_y").fi(_: VectorD))

            var f_tForm = MatrixD (f_tForm(0)(x_fcast)).transpose
            for i <- 1 until n_fEndo do x_fcast_fEndo = x_fcast_fEndo :^+ f_tForm(i)(x_fcast)
            x_fcast_fEndo = tForms("tForm_endo").f(x_fcast_fEndo)
            for i <- 0 until n_fEndo do x_fEndo(i) = x_fcast_fEndo(?, i)
        else
            for i <- 0 until n_fEndo do x_fEndo(i) = tForms("fEndo")(i).f(x_fcast)
        x_fEndo
    end scaleCorrection

end ARX_Symb

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
x_fcast_fEndo:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `ARX_Symb` companion object provides factory methods for the `ARX_Symb` class.
 */
object ARX_Symb extends MakeMatrix4TS

        val (/** Create an `ARX_Symb` object by building an input matrix xy and then calling the
     *  `ARX_Symb` constructor.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param fEndo    the array of functions used to transform endogenous variables
     *  @param fExo     the array of functions used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               fEndo: Array [Transform] = Array (log1pForm),
               fExo: Array [Transform] = Array (log1pForm),
               bakcast: Boolean = false): ARX_Symb =, n_fEndo) = (fEndo.length, fExo.length)
        val (n_fExo, xy)      = buildMatrix (xe, y, hparam, fEndo, fExo, bakcast)
        val tForms = if fname_ == null then formNames (xe.dim2, hparam, n_fEndo, n_fExo) else fname_
        fname
    end apply 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb` object by building an input matrix xy and then calling the
     *  `ARX_Symb` constructor, with rescaling of endogneous and exogenous variable values.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param fEndo    the array of functions used to transform endogenous variables
     *  @param fExo     the array of functions used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     *  @param tForm    the transform for y
     */
    def rescale (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
                 tRng: Range = null, hparam: HyperParameter = hp,
                 fEndo: Array [Transform] = Array (log1pForm),
                 fExo: Array [Transform] = Array (log1pForm),
                 bakcast: Boolean = false,
                 tForm: VectorD | MatrixD => Transform = x => zForm(x)): ARX_Symb =

        val (n_fEndo, n_fExo) = (fEndo.length, fExo.length)
        val (xy, tForms)      = buildMatrix (xe, y, hparam, fEndo, fExo, bakcast, tForm)
        if tForms("tForm_y").getClass.getSimpleName == "zForm" then hp("nneg") = 0
        val y_scl = tForms("tForm_y").f(y)
        val fname = if fname_ == null then formNames (xe.dim2, hparam, n_fEndo, n_fExo) else fname_
        /** Build the input matrix by combining the p + spec columns for the trend and
     *  endogenous variable with the q * xe.dim2 columns for the exogenous variables.
     *  @param xe       the matrix of exogenous variable values
     *  @param y_ypp    the response vector (time series data) and raised to power pp
     *  @param hp_      the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix (xe: MatrixD, y: VectorD, hp_ : HyperParameter, fEndo: Array [Transform],
                     fExo: Array [Transform], bakcast: Boolean,
                     tForm: VectorD | MatrixD => Transform = null): (MatrixD, TransformMap) =
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    p

        val (q, spec, lwave, cross, y_fEndo) = (hp_("p").toInt, hp_("q").toInt, hp_("spec").toInt, hp_("lwave").toDouble, hp_("cross").toInt == 1)

        // apply transformations to the endogenous and exogenous variables
        var y_scl = MatrixD(fEndo(0).f(y)).transpose
        for i <- 1 until fEndo.length do y_fEndo = y_fEndo :^+ fEndo(i).f(y)   // add each transformation of the endogenous variable

        var tForms = y

        val tForm_y: TransformMap =
        if tForm != null then
            val tForm_endo = tForm(y)
            y_scl = tForm_y.f(y)
            val x_endo = tForm(y_fEndo)
            y_fEndo = tForm_endo.f(y_fEndo)
            Map("tForm_y" -> tForm_y, "tForm_endo" -> tForm_endo, "fEndo" -> fEndo, "fExo" -> fExo)
        else
            Map("tForm_y" -> null, "fEndo" -> fEndo, "fExo" -> fExo)

        val xy = y_scl +^: y_fEndo

        // add trend terms and terms for the endogenous variable
        var xe_bfill = makeMatrix4T (y, spec, lwave, bakcast) ++^
                 makeMatrix4L (x_endo, p, bakcast)                             // lagged linear terms

        if xe.dim2 > 0 then
            val x_exo = new MatrixD(xe.dim, xe.dim2)
            for j <- xe.indices2 do xe_bfill(?, j) = backfill(xe(?, j))
            var tForm_exo = xe_bfill
            for i <- fExo.indices do x_exo = x_exo ++^ fExo(i).f(xe_bfill)     // add each transformation of the exogenous variable
            // add cross terms of the endogenous and exogenous variables
            if cross then x_exo = x_exo ++^ y *~: xe_bfill                     // element-wise multiplication of vector y and matrix xe

            if tForm != null then
                val /** Form an array of names for the features included in the model.
     *  @param n_exo  the number of exogenous variable
     *  @param hp_    the hyper-parameters
     *  @param n_fEn  the number of functions used to map endogenous variables
     *  @param n_fEx  the number of functions used to map exogenous variables
     */
    def formNames (n_exo: Int, hp_ : HyperParameter, n_fEn: Int, n_fEx: Int): Array [String] = = tForm(x_exo)
                x_exo = tForm_exo.f(x_exo)
            xy = xy ++^ makeMatrix4L (x_exo, q, bakcast)

        (xy, tForms)
    end buildMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    spec
        val (p, q, cross, names) = (hp_("cross").toInt, hp_("p").toInt, hp_("q").toInt, hp_("cross").toInt)
        val /** The `aRX_SymbTest3` main function tests the `ARX_Symb` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest3
 */
@main def aRX_SymbTest3 (): Unit = = ArrayBuffer [String] ()
        for i <- 0 until n_fEn; j <- p to 1 by -1 do names += s"f$i(yl$j)"           // function lags endo terms

        for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k"           // exo lag terms
        for i <- 0 until n_fEx do
            for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"g$i(xe${j}l$k)"  // function lags exo terms

        if cross == 1 then
            for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k*yl$k"  // lagged cross terms

        MakeMatrix4TS.formNames (spec, p) ++ names.toArray
    end formNames

end ARX_Symb


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest` main function tests the `ARX_Symb` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest
 *
@main def aRX_SymbTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Symb (y, hh)                                          // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRX_SymbTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest2` main function tests the `ARX_Symb` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest2
 *
@main def aRX_SymbTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Symb (y, hh)                                          // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRX_SymbTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
exo_vars

//  val exo_vars  = NO_EXO
    val xxe  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (yy, xe) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val y = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val hh  = yy(0 until 116)                                            // clip the flat end
    val pp = 6                                                          // maximum forecasting horizon
    val ff = 1.5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)
//  hp("cross") = 1                                                     // 1 => add cross terms

    val gg = Array [Transform] (powForm (VectorD (pp)))
    val mod = Array [Transform] ()

    for p <- 6 to 6; s <- 1 to 1; q <- 6 to 6 do                        // number of lags; trend; number of exo lags
        hp("p")    = p                                                  // endo lags
        hp("q")    = q                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5

        val /** The `aRX_SymbTest4` main function tests the `ARX_Symb` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest4
 */
@main def aRX_SymbTest4 (): Unit = = ARX_Symb (xe, y, hh, fEndo = ff, fExo = gg)           // create model for time series data
        mod.inSampleTest ()                                             // In-sample Testing
        println (mod.summary ())                                        // statistical summary of fit
    end for

end aRX_SymbTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
exo_vars

    val xxe  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (yy, xe) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val y = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val hh  = yy(0 until 116)                                            // clip the flat end
    val pp = 6                                                          // maximum forecasting horizon
    val ff = 1.5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)
//  hp("cross") = 1                                                     // 1 => add cross terms

    val gg = Array [Transform] (powForm (VectorD (pp)))
    val mod = Array [Transform] ()

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                        // number of lags (endo, exo); trend
        hp("p")    = p                                                  // endo lags
        hp("q")    = q                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5

        val mod = ARX_Symb (xe, y, hh, fEndo = ff, fExo = gg)           // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.forecastAll ()                                              // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (mod.getY, mod.getYf)

        banner ("rollValidate")
        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (mod.getY, mod.getYf, Forecaster.teRng (y.dim), 0)   // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_SymbTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest5` main function tests the `ARX_Symb` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  This version performs feature selection.
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest5
 *
@main def aRX_SymbTest5 (): Unit =

    val exo_vars  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    val p  = 6 
    val q  = 6
    hp("p")     = p                                                     // endo lags
    hp("q")     = q                                                     // exo lags
    hp("spec")  = 5                                                     // trend specification: 0, 1, 2, 3, 5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)
    hp("cross") = 1                                                     // 1 => add cross terms
    hp("lambda") = 1.0                                                  // regularization/shrinkage parameter

    val ff = Array (powTo (1.5), powTo (0.5), log1p, sin, cos)          // functions to apply to endo lags 
    val gg = Array (powTo (1.5), powTo (0.5), log1p, sin, cos)          // functions to apply to exo lags

    val mod = ARX_Symb (xe, y, hh, fEndo = ff, fExo = gg)               // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset
    println (mod.summary ())                                            // statistical summary of fit

    mod.setSkip(0)
    mod.rollValidate ()                                                 // TnT with Rolling Validation
    mod.diagnoseAll (mod.getY, mod.getYf, Forecaster.teRng(y.dim), 0)

    banner ("Feature Selection Technique: Stepwise")
    val (cols, rSq) = mod.stepwiseSelAll ()                             // R^2, R^2 bar, sMAPE, R^2 cv
//  val (cols, rSq) = mod.backwardElimAll ()                            // R^2, R^2 bar, sMAPE, R^2 cv
    val k = cols.size
    println (s"k = $k")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "R^2 cv"),
               s"R^2 vs n for ${mod.modelName}", lines = true)
    println (s"rSq = $rSq")

end aRX_SymbTest5
 */

