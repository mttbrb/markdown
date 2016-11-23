module markdown


type MarkdownDocument = List<MarkdownBlock>

and MarkdownBlock = 
  | Heading of int * MarkdownSpans
  | Paragraph of MarkdownSpans
  | CodeBlock of List<string>

and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan =
  | Literal of string
  | InlineCode of string
  | Strong of MarkdownSpans
  | Emphasis of MarkdownSpans
  | Hyperlink of MarkdownSpans * string


let toString chars =
  System.String(chars |> Array.ofList)


let rec parseInlineBody acc = function
  | '`'::rest -> Some(List.rev acc,rest)
  | c::chars -> parseInlineBody (c::acc) chars
  | [] -> None


let parseInline = function
  | '`'::chars -> parseInlineBody [] chars
  | _ -> None


let rec parseSpans acc chars = seq {
    let emitLiteral = seq {
        if acc <> [] then
          yield acc |> List.rev |> toString |> Literal }
    
    match parseInline chars, chars with
    | Some (body, chars), _ ->
      yield! emitLiteral
      yield body |> toString |> InlineCode
      yield! parseSpans [] chars
    | _, c::chars ->
      yield! parseSpans (c::acc) chars
    | _, [] ->
      yield! emitLiteral }


[<EntryPoint>]
let main argv =
  let input = "`code` and `more code`" |> List.ofSeq
  let parsed = parseSpans [] input
  printfn "%A" parsed
  0 // return an integer exit code
