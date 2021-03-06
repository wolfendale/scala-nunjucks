package wolfendale.nunjucks

import cats.data.NonEmptyList
import fastparse._
import wolfendale.nunjucks.expression.Parser
import wolfendale.nunjucks.expression.syntax.AST

object TemplateParser {

  private def openExpression[_: P]  = P("{{" ~~ "-".?)
  private def closeExpression[_: P] = P("-".? ~~ "}}")
  private def openComment[_: P]     = P("{#" ~~ "-".?)
  private def closeComment[_: P]    = P("-".? ~~ "#}")
  private def openTag[_: P]         = P("{%" ~~ "-".?)
  private def closeTag[_: P]        = P("-".? ~~ "%}")

  def partial[_: P]: P[TemplateNode.Partial] = {

    def expression = {

      import MultiLineWhitespace._
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

      import NoWhitespace._
      P(
        (`if` ~ partial).map(TemplateNode.If.ConditionalContent.tupled)
          ~ (elif ~ partial).map(TemplateNode.If.ConditionalContent.tupled).rep
          ~ (`else` ~ partial).?
          ~ endIf
      )
        .map {
          case (x, xs, e) =>
            TemplateNode.If(x +: xs, e)
        }
    }

    def switchTag: P[TemplateNode.Switch] = {
      def switch = {
        import MultiLineWhitespace._
        P( openTag ~ "switch" ~/ Parser.expression ~ closeTag)
      }

      def `case` = {
        import MultiLineWhitespace._
        P( openTag ~ "case" ~/ Parser.expression ~ closeTag)
      }

      def `default` = {
        import MultiLineWhitespace._
        P( openTag ~ "default" ~ closeTag)
      }

      def endSwitch = {
        import MultiLineWhitespace._
        P( openTag ~ "endswitch" ~ closeTag)
      }

      import MultiLineWhitespace._
      P(
        switch
          ~ (`case` ~ partial).map(a => TemplateNode.Switch.ConditionalContent(Seq(a._1), a._2)).rep
          ~ (`default` ~ partial).?
          ~ endSwitch)
        .map {
          case (x, xs, e) =>
            TemplateNode.Switch(x, xs, e)
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

      def `else` = {
        import SingleLineWhitespace._
        P(openTag ~ "else" ~ closeTag)
      }

      import NoWhitespace._
      P(open ~ partial ~ (`else` ~ partial).? ~ close).map(TemplateNode.For.tupled)
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

        import NoWhitespace._
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
        import NoWhitespace._
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

      import NoWhitespace._
      P(open ~ partial ~ close)
    }.map(TemplateNode.Macro.tupled)

    def callTag = {

      def identifier = {
        import SingleLineWhitespace._
        sealed abstract class Access
        final case class DirectAccess(identifier: AST.Identifier) extends Access
        final case class ComputedAccess(identifier: AST.Expr)     extends Access
        def directAccess   = P("." ~ Parser.identifier).map(DirectAccess)
        def computedAccess = P("[" ~ Parser.expression ~ "]").map(ComputedAccess)
        P(Parser.identifier ~ (directAccess | computedAccess).rep).map {
          case (lhs, chunks) =>
            chunks.foldLeft[AST.Expr](lhs) {
              case (l, DirectAccess(identifier)) =>
                AST.Access(l, identifier)
              case (l, ComputedAccess(expr)) =>
                AST.ComputedAccess(l, expr)
            }
        }
      }

      def open = {
        import SingleLineWhitespace._
        P(
          openTag ~ "call" ~ ("(" ~ (Parser.identifier ~ ("=" ~ Parser.expression).?).rep(sep = ",").map(_.toMap) ~ ")").?.map(_.getOrElse(Map.empty)) ~ identifier ~ "(" ~ ((Parser.identifier ~ "=").? ~ Parser.expression)
            .rep(sep = ",") ~ ")" ~ closeTag)
      }

      def close = {
        import SingleLineWhitespace._
        P(openTag ~ "endcall" ~ closeTag)
      }

      import NoWhitespace._
      P(open ~ partial ~ close).map(TemplateNode.Call.tupled)
    }

    def includeTag = {
      import SingleLineWhitespace._
      P(openTag ~ "include" ~ Parser.expression ~ ("ignore" ~ "missing").!.?.map(_.isDefined) ~ closeTag)
        .map(TemplateNode.Include.tupled)
    }

    def withContext = {
      import SingleLineWhitespace._
      def withContext    = P("with").map(_ => true)
      def withoutContext = P("without").map(_ => false)
      P(" " ~ (withoutContext | withContext) ~ "context").?.map(_.getOrElse(false))
    }

    def importTag = {
      import SingleLineWhitespace._
      P(openTag ~ "import" ~ Parser.expression ~ "as" ~ Parser.identifier ~~ withContext ~ closeTag)
    }.map(TemplateNode.Import.tupled)

    def fromTag = {
      import SingleLineWhitespace._
      P(
        openTag ~ "from" ~ Parser.expression ~ "import" ~ (Parser.identifier ~ ("as" ~ Parser.identifier).?)
          .rep(1, sep = ",") ~~ withContext ~ closeTag)
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

      import NoWhitespace._
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

      import NoWhitespace._
      P(open ~ partial ~ close)
        .map(TemplateNode.Filter.tupled)
    }

    def extendsTag = {
      import SingleLineWhitespace._
      P(openTag ~ "extends" ~ Parser.expression ~ closeTag)
        .map(TemplateNode.Extends)
    }

    def tag: P[TemplateNode.Tag] =
      P(ifTag | switchTag | forTag | setTag | verbatimTag | macroTag | extendsTag | callTag | includeTag | importTag | fromTag | blockTag | filterTag)

    import NoWhitespace._
    def literal = {
      P((!(openTag | openExpression | openComment) ~~ AnyChar).rep(1).!)
        .map(TemplateNode.Literal)
    }

    P(expression | comment | tag | literal).rep.map(TemplateNode.Partial)
  }

  def template[_: P]: P[Template] = {
    import NoWhitespace._
    P(partial ~ End).map(Template)
  }
}

object NunjucksWhitespace {

  // TODO: fix weird issue with a single leading newline?!
  // TODO: potentially refactor out mutable state
  // TODO: include whitespace rules for inside tags and expressions?
  implicit val whitespace: (ParsingRun[_] => ParsingRun[Unit]) = { implicit ctx =>
    val close = "-[}#%]}"
    val open  = "\\{[{#%]-"
    val input = ctx.input
    var index = ctx.index
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
