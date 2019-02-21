package org.jetbrains.plugins.hocon.ref

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.hocon.psi.HIncludeTarget

abstract class AbstractHoconIncludeResolutionInspection(forRequired: Boolean) extends LocalInspectionTool {
  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    new PsiElementVisitor {
      override def visitElement(element: PsiElement): Unit = element match {
        case hit: HIncludeTarget if hit.parent.flatMap(_.parent).exists(_.required == forRequired) =>
          hit.getFileReferences.foreach { ref =>
            if (!ref.isSoft && ref.multiResolve(false).isEmpty) {
              holder.registerProblem(ref, ProblemsHolder.unresolvedReferenceMessage(ref),
                ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
            }
          }
        case _ =>
          super.visitElement(element)
      }
    }
}

class HoconIncludeResolutionInspection extends AbstractHoconIncludeResolutionInspection(false)
class HoconRequiredIncludeResolutionInspection extends AbstractHoconIncludeResolutionInspection(true)