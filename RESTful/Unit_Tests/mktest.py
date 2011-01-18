#!/usr/bin/python -t
# vim:tabstop=4:softtabstop=4:shiftwidth=4:expandtab

import sys

def instantiate_template(pkg, tmpl, fn):
   f_in = open(tmpl, 'r')
   f_out = open(fn, 'w')

   content = f_in.read()
   content = content.replace('<PKG>',pkg)
   f_out.write(content)

   f_in.close()
   f_out.close()


def filename(pkg):
   return pkg.lower().replace('.','-')


def ada_filenames(pkg):
   fn = filename(pkg)
   return (fn +'.ads', fn +'.adb')


def create_test(pkg):
   test_pkg = pkg + '.Test'
   suite_pkg = test_pkg + '.Suite'
   (test_ads, test_adb) = ada_filenames(test_pkg)
   (suite_ads, suite_adb) = ada_filenames(suite_pkg)


   instantiate_template(pkg, 'test.ads.template', test_ads)
   instantiate_template(pkg, 'test.adb.template', test_adb)

   instantiate_template(pkg, 'suite.ads.template', suite_ads)
   instantiate_template(pkg, 'suite.adb.template', suite_adb)

   print 'Created files '
   print '  * ', test_ads
   print '  * ', test_adb
   print '  * ', suite_ads
   print '  * ', suite_adb
   print 'You still need to implement', test_adb, 'and '
   print 'add with', suite_pkg, 'to composite_suite.adb.'


args = sys.argv[1:]
for pkg in args:
   create_test(pkg)


