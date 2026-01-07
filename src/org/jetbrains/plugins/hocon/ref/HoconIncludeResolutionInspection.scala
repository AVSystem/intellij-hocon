package org.jetbrains.plugins.hocon
package ref

import psi.HIncludeTarget

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.{PsiElement, PsiElementVisitor}

enum AbstractHoconIncludeResolutionInspection(forRequired: Boolean) extends LocalInspectionTool {
  case HoconIncludeResolutionInspection extends AbstractHoconIncludeResolutionInspection(false)

  case HoconRequiredIncludeResolutionInspection extends AbstractHoconIncludeResolutionInspection(true)

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    new PsiElementVisitor {
      override def visitElement(element: PsiElement): Unit = element match {
        case hit: HIncludeTarget if hit.parent.parent.required == forRequired =>
          hit.getFileReferences.foreach { ref =>
            if (!ref.isSoft && ref.multiResolve(false).isEmpty) {
              holder.registerProblem(
                ref,
                ProblemsHolder.unresolvedReferenceMessage(ref),
                ProblemHighlightType.LIKE_UNKNOWN_SYMBOL,
              )
            }
          }
        case _ =>
          super.visitElement(element)
      }
    }
}
