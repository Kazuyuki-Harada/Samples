/*
 * ScalaEngine Ver 1.0
 * for Scala 2.9.2
 * ------------------------------------------------------------------------
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at 
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sample;
import javax.script.{
    Bindings,
    SimpleBindings,
    SimpleScriptContext,
    AbstractScriptEngine,
    ScriptException,
    ScriptContext,
    ScriptEngineFactory};

import scala.io.Source;
import scala.tools.nsc._;
import scala.tools.nsc.settings._;
import scala.tools.nsc.interpreter.IMain;
import scala.tools.nsc.interpreter.Results;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.Proxy.isProxyClass;

case object ErrorOccured;
case object Incomplete;

object ScalaEngine {
  def createSettings(cp: String, sets: Seq[String]) = {
    val s = new Settings(print _);
    if (cp==null) s.usejavacp.value=true
    else s.classpath.value = cp;
    for (b<-sets) {
      s.BooleanSetting(b, null);
    }
    s
  }
}

/**
 * @param scala compiler setting
 */
class ScalaEngine(bindings: Bindings, settings: Settings, out: PrintStream) extends AbstractScriptEngine(bindings) {
  /**
   * @param cp classpath for scala
   * @param boolsets BooleanSetting string sequence such as Seq("-Xmx1000m")
   */
  def this(cp: String, boolsets: Seq[String]) = this(new SimpleBindings(), ScalaEngine.createSettings(cp, boolsets), Console.out);
  def this(cp: String) = this(cp, Nil);
  override def getFactory() : ScriptEngineFactory = null;
  override def createBindings() = new SimpleBindings();

  val iMain = new IMain(settings, new PrintWriter(out));
  init();

  iMain.beQuietDuring(iMain.bind("_scalaEngine", this.getClass().getName, this));
  iMain.beQuietDuring(iMain.bind("_out", "java.io.PrintStream", out));
  var _results = new Array[Any](1);

  override def eval(r: java.io.Reader) : AnyRef = {
    val br = new java.io.BufferedReader(r);
    var end = false;
    var last : Any = null;
    while (!end) {
      val line = br.readLine;
      if (line == null) end = true else {
        last = evaluate(line, false);
      }
    }
    last match {
      case r : AnyRef => r;
      case _ => null;
    }
  }
  override def eval(r: java.io.Reader, bindings: Bindings) : AnyRef = {
    throw new ScriptException("unsupported eval with bindings");
  }
  override def eval(r: java.io.Reader, context: ScriptContext) : AnyRef = {
    throw new ScriptException("unsupported eval with context");
  }
  override def eval(s: String) : AnyRef = {
    evaluate(s) match {
      case a: AnyRef => a;
      case _ => null;
    }
  }
  override def eval(s: String, bindings: Bindings) : AnyRef = {
    throw new ScriptException("unsupported eval with bindings");
  }
  override def eval(s: String, context: ScriptContext) : AnyRef = {
    throw new ScriptException("unsupported eval with context");
  }
  override def get(key: String) : AnyRef = getAnyRef(key);
  override def put(name: String, value: Any) {
    value match {
      case null => evaluate("val " + name + " = null;");
      case s:String => evaluate("val " + name + " = \"\"\"" + s + "\"\"\";");
      case cls:Class[_] => bind(name, "Class[_]", cls);
      case obj: AnyRef =>
        if (obj.isInstanceOf[Array[_]]) {
          val cname = obj.getClass().getCanonicalName();
          if (!cname.endsWith("[]")) 
throw new IllegalStateException("unsupported array class:" + obj.getClass());

          val typename = "Array[" + cname.substring(0, cname.length()-2) + "]";
          bind(name, typename, obj);
        } else if (isProxyClass(obj.getClass())) {
          val interfaces = obj.getClass().getInterfaces();
          if (interfaces.length != 1) 
throw new IllegalArgumentException("unsupport Proxy class having over one interfaces : " + obj);

          bind(name, interfaces(0).getName(), obj);
        } else {
          bind(name, obj.getClass().getName(), obj);
        }

      case n:Int=>evaluate("val " + name + " : Int =" + n);
      case n:Long=>evaluate("val " + name + " : Long =" + n);
      case n:Byte=>evaluate("val " + name + " : Byte =" + n);
      case n:Short=>evaluate("val " + name + " : Short =" + n);
      case n:Char=>evaluate("val " + name + " ='" + n + "'");
      case n:Float=>evaluate("val " + name + " :Float =" + n);
      case n:Double=>evaluate("val " + name + " :Double =" + n);

      case x =>
        if (x.isInstanceOf[Int]) eval("val " + name + "=" + x);
        else throw new IllegalArgumentException("unsupport put : " + x);
    }
  }
  /**
   * it can put value with a type such as java.util.List[java.lang.String].
   */
  def putWithType(name:String, typename:String, value:Any) {
    if (value==null) evaluate("val " + name + "=null;");
    else bind(name, typename, value);
  }

  /**
   * 
   * @return the value evaluated/ErrorOccured/Incomplete
   */
  def evaluate(line: String, synthetic: Boolean) : Any = {
    // this returns null when None. (for it uses in Java)
    iMain.beQuietDuring(iMain.interpret(line, synthetic)) match {
      case Results.Error => ErrorOccured;
      case Results.Incomplete => Incomplete;
      case Results.Success =>
        val name = iMain.mostRecentVar
        getAny(name);
    }
  }
  def evaluate(line: String) : Any = evaluate(line, false);

  def getAny(name: String) : Any = {
    iMain.beQuietDuring(iMain.interpret("_scalaEngine._results(0) = " + name))
    // the array's length must be over 1
    _results(0);
  }

  def getAnyRef(name: String) : AnyRef = {
    getAny(name) match {
      case null => null;
      case x: AnyRef => x;
      case x : Any =>
        try {
          x.asInstanceOf[AnyRef];
        } catch {
          case e: Throwable => null;
        }
    }
  }
  def init() {
    iMain.initializeSynchronous
  }
  def close() {
    iMain.close
  }
  def bind(key: String, typename: String, value: Any) {
    iMain.beQuietDuring(iMain.bind(key, typename, value));
  }
  def getVariables() : Map[String, AnyRef] = {
    val varnames = for (n<-iMain.definedTerms if (iMain.terms(n.toString()).toString().startsWith("value "))) yield n;
    val length = if (varnames.size == 0) 1 else varnames.size;
    _results = new Array[Any](length);
    val names = new Array[String](length);
    var n = 0;
    val sb = new StringBuilder;
    for (key<-varnames) {
      val keyname = key.toString;
      names(n) = keyname;
      sb.append("_scalaEngine._results(" + n + ") = " + keyname + ";\n");
      n += 1;
    }
    iMain.beQuietDuring(iMain.interpret(sb.toString));
    val retmap = scala.collection.mutable.Map[String, AnyRef]();
    for (i<-0 until length) {
        _results(i) match {
          case x: scala.runtime.BoxedUnit => // it generates res0,res1,...
          case obj: AnyRef => retmap.put(names(i), obj);
          case _ => // Ignoring
        }
    }
    return(retmap.toMap);
  }

}

