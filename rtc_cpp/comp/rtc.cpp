/* RTC:
 *
 * Copyright (C) 2011
 *     Geoffrey Biggs
 *     RT-Synthesis Research Group
 *     Intelligent Systems Research Institute,
 *     National Institute of Advanced Industrial Science and Technology (AIST),
 *     Japan
 *     All rights reserved.
 * Licensed under the Eclipse Public License -v 1.0 (EPL)
 * http://www.opensource.org/licenses/eclipse-1.0.txt
 *
 * Component source file.
 */


#include "rtc.h"


RTC::RTC(RTC::Manager* manager)
    : RTC::DataFlowComponentBase(manager),
    _port_("<PORT_NAME>", _),
    //svc_prov_(),
    //svc_port_("<PORT_NAME>")
{
}


RTC::~RTC()
{
}


RTC::ReturnCode_t RTC::onInitialize()
{
    bindParameter("", _, "");
    /*std::string active_set =
        m_properties.getProperty("configuration.active_config", "default");
    m_configsets.update(active_set.c_str());*/

    comp.addInPort(_port_.getName(), _port_);
    comp.addOutPort(_port_.getName(), _port_);
    //svc_port_.registerProvider("<INSTANCE_NAME>", "<TYPE_NAME>", svc_prov_);
    //comp.addPort(svc_port_);

    return RTC::RTC_OK;
}


/*RTC::ReturnCode_t RTC::onFinalize()
{
    return RTC::RTC_OK;
}*/


/*RTC::ReturnCode_t RTC::onActivated(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}*/


/*RTC::ReturnCode_t RTC::onDeactivated(RTC::UniqueId ec_id)
{
    return RTC::RTC_OK;
}*/


/*RTC::ReturnCode_t RTC::onExecute(RTC::UniqueId ec_id)
{

    return RTC::RTC_OK;
}*/


static const char* spec[] =
{
    "implementation_id", "<COMP>",
    "type_name",         "<COMP_TYPE>",
    "description",       "",
    "version",           "1.0",
    "vendor",            "Geoffrey Biggs, AIST",
    "category",          "",
    "activity_type",     "PERIODIC",
    "kind",              "DataFlowComponent",
    "max_instance",      "1",
    "language",          "C++",
    "lang_type",         "compile",
    // Configuration variables
    // Widget
    //"conf.__widget__.", "text",
    //"conf.__widget__.", "spin",
    // Constraints
    ""
};

extern "C"
{
    void rtc_init(RTC::Manager* manager)
    {
        coil::Properties profile(spec);
        manager->registerFactory(profile, RTC::Create<RTC>,
                RTC::Delete<RTC>);
    }
};

