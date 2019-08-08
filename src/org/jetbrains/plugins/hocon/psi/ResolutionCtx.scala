package org.jetbrains.plugins.hocon
package psi

case class OpenInclude(
  include: HInclude,
  targetFile: HoconPsiFile
)

case class OpenSubstitution(
  path: HPath,
  openIncludes: List[OpenInclude]
)

case class ResolutionCtx(
  fromFile: HoconPsiFile,
  followIncludes: Boolean,
  openIncludes: List[OpenInclude] = Nil,
  openSubstitutions: List[OpenSubstitution] = Nil
) {
  def withInclude(openInclude: OpenInclude): ResolutionCtx =
    copy(openIncludes = openInclude :: openIncludes)

  def withSubstitution(path: HPath): ResolutionCtx =
    copy(openIncludes = Nil, openSubstitutions = OpenSubstitution(path, openIncludes) :: openSubstitutions)
}
