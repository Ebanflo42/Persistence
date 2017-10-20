module HasseNode

type HasseNode(Vertices : int list, Parents : HasseNode list, Children : HasseNode list) =
  member this.Vertices = Vertices
  member this.Parents  = Parents
  member this.Children = Children

  member this.checkNodeChilds =
    let dim = Vertices.Length
    if dim = 1 && Children = [] then true
    else Children
      |> List.map (fun nodes -> nodes.Vertices.Length)
        |> List.forall (fun x -> x = dim - 1)

  member this.checkNodeParens =
    if Parents = [] then true
    else Parents
      |> List.map (fun nodes -> nodes.Vertices.Length)
        |> List.forall (fun x -> x = Vertices.Length + 1)

  member this.checkNode =
    this.checkNodeChilds && this.checkNodeParens
