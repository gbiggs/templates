#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- Python -*-

'''<Project>

Copyright (C) 2010
    Geoffrey Biggs
    RT-Synthesis Research Group
    Intelligent Systems Research Institute,
    National Institute of Advanced Industrial Science and Technology (AIST),
    Japan
    All rights reserved.
Licensed under the Eclipse Public License -v 1.0 (EPL)
http://www.opensource.org/licenses/eclipse-1.0.txt

<Description>

'''


import OpenRTM_aist
import RTC
import sys


class Comp(OpenRTM_aist.DataFlowComponentBase):
    def __init__(self, manager):
        OpenRTM_aist.DataFlowComponentBase.__init__(self, manager)

    def onInitialize(self):
        #self._ = 
        #self.__port = OpenRTM_aist.InPort('', self._)
        #self.addInPort('', self.__port)
        #self._ = 
        #self.__port = OpenRTM_aist.OutPort('', self._)
        #self.addOutPort('', self.__port)
        return RTC.RTC_OK

    #def onActivated(self, ec_id):
        #return RTC.RTC_OK

    #def onDeactivated(self, ec_id):
        #return RTC.RTC_OK

    #def onExecute(self, ec_id):
        #return RTC.RTC_OK


comp_spec = ['implementation_id', '',
        'type_name', '',
        'description', '',
        'version', '1.0',
        'vendor', 'Geoffrey Biggs',
        'category', '',
        'activity_type', 'DataFlowComponent',
        'max_instance', '1',
        'language', 'Python',
        'lang_type', 'script',
        '']


def CompInit(manager):
    profile = OpenRTM_aist.Properties(defaults_str=comp_spec)
    manager.registerFactory(profile, Comp, OpenRTM_aist.Delete)


def ModuleInit(manager):
    CompInit(manager)
    manager.createComponent('')


def main():
    mgr = OpenRTM_aist.Manager.init(sys.argv)
    mgr.setModuleInitProc(ModuleInit)
    mgr.activateManager()
    mgr.runManager()


if __name__ == '__main__':
    main()

