# Custom errors that can be raised during the fmodpy process

class BadKeywordArgument(Exception): pass
class BuildAndLinkError(Exception): pass
class CompileError(Exception): pass
class FortranError(Exception): pass
class IllegalConfiguration(Exception): pass
class LinkError(Exception): pass
class NameError(Exception): pass
class NotAllowedPath(Exception): pass
class NotSupportedError(Exception): pass
class ParseError(Exception): pass
class SizeError(Exception): pass
class UnrecognizedConfiguration(Exception): pass
