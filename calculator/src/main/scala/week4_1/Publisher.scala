package week4_1

trait Publisher {
  /**
   * Elementos:
   * Lista de subscritos
   *
   * Acciones:
   * Subscribirse
   * UnSubscribirse
   * Publicar
   */

  private var subscriptores: Set[Subscriber] = Set()

  def subscribirse(sub: Subscriber): Unit =
    subscriptores += sub

  def unSubscribirse(sub: Subscriber): Unit =
    subscriptores -= sub

  def publicar() = subscriptores.foreach( _.handler)
}
