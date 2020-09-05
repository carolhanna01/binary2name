EXTRA_DIST = $(man_MANS) man_generate

MAN3 = WordContext.3 WordList.3 WordDict.3 WordListOne.3 \
	WordKey.3 WordKeyInfo.3 WordType.3 \
	WordDBInfo.3 WordRecordInfo.3 WordRecord.3 \
	WordReference.3 WordCursor.3 WordCursorOne.3 \
	WordMonitor.3 Configuration.3 \
	mifluz.3
MAN3_FILES = $(patsubst %.3,%.h,$(MAN3))

MAN1 =  htdb_dump.1 htdb_stat.1 htdb_load.1 \
	mifluzdump.1 mifluzload.1 mifluzsearch.1 \
	mifluzdict.1 mifluz-config.1

MAN1_FILES = $(patsubst %.1,%.cc,$(MAN1))

man_MANS = $(MAN1) $(MAN3)

MAN_FILES = $(MAN1_FILES) $(MAN3_FILES)

#
# Create all ${man_MANS} files in the current directory
#
mans:
	$(PERL) man_generate --format=man $(MAN_FILES)

#
# Create the ../doc/reference.texinfo file
#
# Once this is generated do the following:
# in ../doc/mifluz.texinfo chop the @chapter reference completely
# and replace it with the ../doc/reference.texinfo content. Run
# C-c C-u C-a then C-c C-u C-e then C-c C-u m to update the menus.
#
texi:
	$(PERL) man_generate --format=texi $(MAN_FILES)

#
# Create the ../mifluz.conf file
#
conf:
	$(PERL) man_generate --format=conf $(MAN_FILES)
