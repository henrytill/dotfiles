import net.ceedubs.sbtctags.CtagsKeys

CtagsKeys.ctagsParams ~= (default => default.copy(tagFileName = "TAGS", extraArgs = "-e" +: default.extraArgs))
