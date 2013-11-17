type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock =
  | Heading of int * MarkdownSpans
  | Paragraph of MarkdownSpans
  | CodeBlock of list<string>

and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan =
  | Literal of string
  | InlineCode of string
  | Strong of MarkdownSpans
  | Emphasis of MarkdownSpans
  | HyperLink of MarkdownSpans * string


let rec parseInlineBody acc =  function
  | '`'::rest ->
    Some(List.rev acc, rest)
  | c::charts ->
    parseInlineBody (c::acc) chars
  | [] -> None

let parseInline = function
  | '`'::chars ->
    parseInlineBody [] chars
  | _ -> None
