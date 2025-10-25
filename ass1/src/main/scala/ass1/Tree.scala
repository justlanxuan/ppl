package ass1

enum Tree[K, V] extends TreeInterface[K, V]:
  private case Leaf(key: K, payload: V)
  private case Node(key: K, children: List[Tree[K, V]])

  import Tree.Path
  // done
  def contains(path: Path[K]): Boolean = this match
    case(Leaf(_,_)) => path match 
      case Path(Nil) => true
      case Path(head :: tail) => false
    case(Node(key, children)) => path match
      case Path(Nil) => true
      case Path(head :: tail) =>
        val idx = (children.map(_.key)).indexOf(head)
        if idx == -1 then 
          false
        else 
          children(idx).contains(Path(tail))
  // done
  def get(path: Path[K]): Option[Either[List[K], V]] = this match
    case(Leaf(_,payload)) => path match
      case Path(Nil) => Some(Right(payload))
      case Path(head :: tail) => None
    case(Node(k,children)) => path match
      case Path(Nil) => Some(Left(children.map(_.key)))
      case Path(head :: tail) =>
        val idx = (children.map(_.key)).indexOf(head)
        if idx == -1 then
          None
        else
          children(idx).get(Path(tail))
  // done
  def flatten: List[(Path[K], V)] = this match
    case Leaf(key, payload) =>
      List((Path(List(key)), payload))
    
    case Node(key, children) =>
      children.flatMap { child =>
        child.flatten.map { case (Path(segments), value) =>
          (Path(key :: segments), value)
        }
      }

  def updated(path: Path[K]): Tree[K, V] = 
    updatedHelper(path, None, isRoot = true)

  def updated(path: Path[K], payload: V): Tree[K, V] = 
    updatedHelper(path, Some(payload), isRoot = true)

  private def updatedHelper(path: Path[K], payloadOpt: Option[V], isRoot: Boolean): Tree[K, V] = 
    (this, path) match
      case (_, Path(Nil)) =>
        payloadOpt match
          case None =>
            Node(this.key, Nil)
          case Some(payload) =>
            if isRoot then
              throw Tree.IllegalPathException
            else
              Leaf(this.key, payload)
      
      case (Leaf(key, _), Path(head :: tail)) =>
        val newChild = payloadOpt match
          case None => Tree(Path(head :: tail))
          case Some(p) => Tree(Path(head :: tail), p)
        Node(key, List(newChild))
      
      case (Node(key, children), Path(head :: tail)) =>
        val idx = children.map(_.key).indexOf(head)
        
        if idx == -1 then
          val newChild = payloadOpt match
            case None => Tree(Path(head :: tail))
            case Some(p) => Tree(Path(head :: tail), p)
          Node(key, children :+ newChild)
        else
          val updatedChild = children(idx).updatedHelper(Path(tail), payloadOpt, isRoot = false)
          val newChildren = children.updated(idx, updatedChild)
          Node(key, newChildren)

  def display: String =
    def helper(tree: Tree[K, V], prefix: String, isLast: Boolean): String =
      val connector = if isLast then "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
      val line = tree match
        case Leaf(key, payload) => s"Leaf($key, $payload)"
        case Node(key, _) => s"Node($key)"
      
      val currentLine = prefix + connector + line
      
      tree match
        case Leaf(_, _) => currentLine
        case Node(_, children) =>
          if children.isEmpty then
            currentLine
          else
            val extension = if isLast then "    " else "\u2502   "
            val childLines = children.zipWithIndex.map { case (child, idx) =>
              helper(child, prefix + extension, idx == children.length - 1)
            }
            currentLine + "\n" + childLines.mkString("\n")
    
    // Root node has no prefix or connector
    this match
      case Leaf(key, payload) => s"Leaf($key, $payload)"
      case Node(key, children) =>
        val rootLine = s"Node($key)"
        if children.isEmpty then
          rootLine
        else
          val childLines = children.zipWithIndex.map { case (child, idx) =>
            helper(child, "", idx == children.length - 1)
          }
          rootLine + "\n" + childLines.mkString("\n")

object Tree extends TreeComp:
  def apply[K, V](key: K): Tree[K, V] = 
    Node(key, Nil)

  def apply[K, V](path: Path[K]): Tree[K, V] = path match
    case Path(Nil) => 
      throw IllegalPathException
    case Path(head :: Nil) => 
      Node(head, Nil)
    case Path(head :: tail) => 
      Node(head, List(Tree(Path(tail))))

  def apply[K, V](path: Path[K], payload: V): Tree[K, V] = path match
    case Path(Nil) => 
      throw IllegalPathException
    case Path(head :: Nil) => 
      Leaf(head, payload)
    case Path(head :: tail) => 
      Node(head, List(Tree(Path(tail), payload)))