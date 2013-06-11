module Data.LanguageTag.Types where

import Data.Text(Text)
import Data.ISO639(Language)
import Data.ISO3166(Country)

{-
from RFC 1766 § 2:

2.  The Language tag

   The language tag is composed of 1 or more parts: A primary language
   tag and a (possibly empty) series of subtags.

   The syntax of this tag in RFC-822 EBNF is:

    Language-Tag = Primary-tag *( "-" Subtag )
    Primary-tag = 1*8ALPHA
    Subtag = 1*8ALPHA

   Whitespace is not allowed within the tag.
-}
{-
We'll compose a Language tag of a 'PrimaryTag', and a 'FirstSubTag', since
primary and the first subtag have rules arround their construction.
all constructors of 'FirstSubTag' should contain an array of 'SubTag's to
hold all subsequent SubTags. 
-}
data LanguageTag = LanguageTag PrimaryTag (Maybe FirstSubTag) deriving (Eq, Show)


{-
from RFC 1766 § 2:
In the primary language tag:

    -    All 2-letter tags are interpreted according to ISO standard
         639, "Code for the representation of names of languages" [ISO
         639].

    -    The value "i" is reserved for IANA-defined registrations

    -    The value "x" is reserved for private use. Subtags of "x"
         will not be registered by the IANA.

    -    Other values cannot be assigned except by updating this
         standard.

   The reason for reserving all other tags is to be open towards new
   revisions of ISO 639; the use of "i" and "x" is the minimum we can do
   here to be able to extend the mechanism to meet our requirements.
-}
data PrimaryTag  = ISO639Tag Language
                 | IANADefinedTag
                 | PrivateTag
                 deriving (Eq, Show)

{-
from RFC 1766 § 2:
   In the first subtag:

    -    All 2-letter codes are interpreted as ISO 3166 alpha-2
         country codes denoting the area in which the language is
         used.

    -    Codes of 3 to 8 letters may be registered with the IANA by
         anyone who feels a need for it, according to the rules in
         chapter 5 of this document.

   The information in the subtag may for instance be:

    -    Country identification, such as en-US (this usage is
         described in ISO 639)

    -    Dialect or variant information, such as no-nynorsk or en-
         cockney

    -    Languages not listed in ISO 639 that are not variants of
         any listed language, which can be registered with the i-
         prefix, such as i-cherokee

    -    Script variations, such as az-arabic and az-cyrillic
-}
data FirstSubTag = ISO3166Alpha2Tag  Country [SubTag]
                 | IANARegisteredTag Text    [SubTag]
                 deriving (Eq, Show) 
{-
from RFC 1766 § 2:

In the second and subsequent subtag, any value can be registered.
-}
newtype SubTag = SubTag Text deriving (Eq, Show)
