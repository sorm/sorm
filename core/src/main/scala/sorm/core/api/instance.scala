package sorm.core.api

trait Instance extends DeclaresSettings {

  type Driver

  def save[e : Entity](e: e): e with Persisted

}
