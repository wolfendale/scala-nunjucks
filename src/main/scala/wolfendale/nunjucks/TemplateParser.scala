package wolfendale.nunjucks

import cats.data.NonEmptyList
import fastparse._
import wolfendale.nunjucks.expression.Parser

object TemplateParser {

  private def openExpression[_: P]  = P("{{" ~~ "-".?)
  private def closeExpression[_: P] = P("-".? ~~ "}}")
  private def openComment[_: P]     = P("{#" ~~ "-".?)
  private def closeComment[_: P]    = P("-".? ~~ "#}")
  private def openTag[_: P]         = P("{%" ~~ "-".?)
  private def closeTag[_: P]        = P("-".? ~~ "%}")

  def partial[_: P]: P[TemplateNode.Partial] = {

    def expression = {

      import SingleLineWhitespace._
      P(openExpression ~ Parser.expression ~ closeExpression).map(TemplateNode.Expression)
    }

    def comment = {
      import MultiLineWhitespace._
      P(openComment ~ (!closeComment ~ AnyChar).rep.! ~ closeComment)
        .map(TemplateNode.Comment)
    }

    def ifTag = {

      def `if` = {
        import SingleLineWhitespace._
        P(openTag ~ "if" ~/ Parser.expression ~ closeTag)
      }

      def elif = {
        import SingleLineWhitespace._
        P(openTag ~ StringIn("elif", "elseif") ~/ Parser.expression ~ closeTag)
      }

      def `else` = {
        import SingleLineWhitespace._
        P(openTag ~ "else" ~ closeTag)
      }

      def endIf = {
        import SingleLineWhitespace._
        P(openTag ~ "endif" ~ closeTag)
      }

      import NunjucksWhitespace._
      P(
        (`if` ~ partial)
          .map(TemplateNode.If.ConditionalContent.tupled) ~ (elif ~ partial)
          .map(TemplateNode.If.ConditionalContent.tupled)
          .rep ~ (`else` ~ partial).? ~ endIf)
        .map {
          case (x, xs, e) =>
            TemplateNode.If(x +: xs, e)
        }
    }

    def forTag = {

      def open = {
        import SingleLineWhitespace._
        P(
          openTag ~ "for" ~ Parser.identifier
            .rep(1, sep = ",") ~ "in" ~ Parser.expression ~ closeTag)
      }

      def close = {
        import SingleLineWhitespace._
        P(openTag ~ "endfor" ~ closeTag)
      }

      import NunjucksWhitespace._
      P(open ~ partial ~ close).map(TemplateNode.For.tupled)
    }

    def setTag: P[TemplateNode.Tag] = {

      def set = {
        import SingleLineWhitespace._
        P(openTag ~ "set" ~ Parser.identifier ~ ("," ~ Parser.identifier).rep ~ "=" ~ Parser.expression ~ closeTag)
          .map {
            case (x, xs, expr) =>
              TemplateNode.Set(NonEmptyList(x, xs.toList), expr)
          }
      }

      def setBlock = {

        def open = {
          import SingleLineWhitespace._
          P(openTag ~ "set" ~ Parser.identifier ~ ("," ~ Parser.identifier).rep ~ closeTag)
            .map { case (x, xs) => NonEmptyList(x, xs.toList) }
        }

        def close = {
          import SingleLineWhitespace._
          P(openTag ~ "endset" ~ closeTag)
        }

        import NunjucksWhitespace._
        P(open ~ partial ~ close)
      }.map(TemplateNode.SetBlock.tupled)

      P(set | setBlock)
    }

    def verbatimTag = {

      def open(tag: String) = {
        import SingleLineWhitespace._
        P(openTag ~ tag ~ closeTag)
      }

      def close(tag: String) = {
        import SingleLineWhitespace._
        P(openTag ~ s"end$tag" ~ closeTag)
      }

      def verbatim(tag: String) = {
        import NunjucksWhitespace._
        P(open(tag) ~ (!close(tag) ~ AnyChar).rep.! ~ close(tag))
      }

      P(verbatim("verbatim") | verbatim("raw")).map(TemplateNode.Verbatim)
    }

    def macroTag = {

      def open = {
        import SingleLineWhitespace._
        P(
          openTag ~ "macro" ~ Parser.identifier ~ "(" ~ (Parser.identifier ~ ("=" ~ Parser.expression).?)
            .rep(sep = ",")
            .map(_.toMap) ~ ")" ~ closeTag)
      }

      def close = {
        import SingleLineWhitespace._
        P(openTag ~ "endmacro" ~ closeTag)
      }

      import NunjucksWhitespace._
      P(open ~ partial ~ close)
    }.map(TemplateNode.Macro.tupled)

    def callTag = {

      def open = {
        import SingleLineWhitespace._
        P(
          openTag ~ "call" ~ Parser.identifier ~ "(" ~ ((Parser.identifier ~ "=").? ~ Parser.expression)
            .rep(sep = ",") ~ ")" ~ closeTag)
      }

      def close = {
        import SingleLineWhitespace._
        P(openTag ~ "endcall" ~ closeTag)
      }

      import NunjucksWhitespace._
      P(open ~ partial ~ close).map(TemplateNode.Call.tupled)
    }

    def includeTag = {
      import SingleLineWhitespace._
      P(openTag ~ "include" ~ Parser.expression ~ ("ignore" ~ "missing").!.?.map(_.isDefined) ~ closeTag)
        .map(TemplateNode.Include.tupled)
    }

    def importTag = {
      import SingleLineWhitespace._
      P(openTag ~ "import" ~ Parser.expression ~ "as" ~ Parser.identifier ~ closeTag)
    }.map(TemplateNode.Import.tupled)

    def fromTag = {
      import SingleLineWhitespace._
      P(
        openTag ~ "from" ~ Parser.expression ~ "import" ~ (Parser.identifier ~ ("as" ~ Parser.identifier).?)
          .rep(1, sep = ",") ~ closeTag)
        .map(TemplateNode.From.tupled)
    }

    def blockTag = {

      def open = {
        import SingleLineWhitespace._
        P(openTag ~ "block" ~ Parser.identifier ~ closeTag)
      }

      def close = {
        import SingleLineWhitespace._
        P(openTag ~ "endblock" ~ closeTag)
      }

      import NunjucksWhitespace._
      P(open ~ partial ~ close)
        .map(TemplateNode.Block.tupled)
    }

    def filterTag = {

      def open = {
        import SingleLineWhitespace._
        P(
          openTag ~ "filter" ~ Parser.identifier ~ ("(" ~ ((Parser.identifier ~ "=").? ~ Parser.expression)
            .rep(sep = ",") ~ ")").?.map(_.getOrElse(Seq.empty)) ~ closeTag)
      }

      def close = {
        import SingleLineWhitespace._
        P(openTag ~ "endfilter" ~ closeTag)
      }

      import NunjucksWhitespace._
      P(open ~ partial ~ close)
        .map(TemplateNode.Filter.tupled)
    }

    def tag: P[TemplateNode.Tag] =
      P(ifTag | forTag | setTag | verbatimTag | macroTag | callTag | includeTag | importTag | fromTag | blockTag | filterTag)

    import NoWhitespace._
    def literal = {
      P((!(openTag | openExpression | openComment) ~~ AnyChar).rep(1).!)
        .map(TemplateNode.Literal)
    }

    P(expression | comment | tag | literal).rep.map(TemplateNode.Partial)
  }

  def extendsTag[_: P] = {
    import SingleLineWhitespace._
    P(openTag ~ "extends" ~ Parser.expression ~ closeTag)
      .map(TemplateNode.Extends)
  }

  def template[_: P]: P[Template] = {
    import NunjucksWhitespace._
    def rootTemplate  = P(partial.? ~ End).map(RootTemplate)
    def childTemplate = P(extendsTag.map(_.expr) ~ partial.? ~ End).map(ChildTemplate.tupled)
    def complexTemplate =
      P(NoCut(partial.map(Some.apply).map(RootTemplate) ~ childTemplate).map(ComplexTemplate.tupled))
    P(complexTemplate | childTemplate | rootTemplate)
  }
}

object NunjucksWhitespace {

  // TODO: fix weird issue with a single leading newline?!
  // TODO: potentially refactor out mutable state
  // TODO: include whitespace rules for inside tags and expressions?
  implicit val whitespace: (ParsingRun[_] => ParsingRun[Unit]) = { implicit ctx =>
    val close  = "-[}#%]}"
    val open   = "\\{[{#%]-"
    val input  = ctx.input
    var index  = ctx.index
    if (input.slice(index - 3, index).matches(close)) {
      while (input.isReachable(index) &&
             (input(index) match {
               case ' ' | '\t' | '\r' | '\n' => true
               case _                        => false
             })) index += 1
      ctx.freshSuccessUnit(index = index)
    } else {
      while (input.isReachable(index) &&
             !input.slice(index, index + 3).matches(open) &&
             (input(index) match {
               case ' ' | '\t' | '\r' | '\n' => true
               case _                        => false
             })) index += 1
      ctx.freshSuccessUnit(index = index)
    }
  }
}
