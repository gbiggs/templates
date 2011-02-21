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
 * Component header file.
 */


#ifndef RTC_H__
#define RTC_H__

#include <rtm/Manager.h>
#include <rtm/DataFlowComponentBase.h>
//#include <rtm/InPort.h>
//#include <rtm/OutPort.h>
//#include <rtm/CorbaPort.h>
//#include "service_impl.h"

using namespace RTC;


// Base exception
class BaseRTCError : public std::runtime_error
{
    public:
        BaseRTCError(const std::string &arg)
            : std::runtime_error(std::string("Base error ") + arg)
        {}
};


class RTC
: public RTC::DataFlowComponentBase
{
    public:
        RTC(RTC::Manager* manager);
        ~RTC();

        virtual RTC::ReturnCode_t onInitialize();
        //virtual RTC::ReturnCode_t onFinalize();
        //virtual RTC::ReturnCode_t onActivated(RTC::UniqueId ec_id);
        //virtual RTC::ReturnCode_t onDeactivated(RTC::UniqueId ec_id);
        //virtual RTC::ReturnCode_t onExecute(RTC::UniqueId ec_id);

    private:
        RTC:: _;
        RTC::InPort<RTC::> _port_;
        RTC:: _;
        RTC::OutPort<RTC::> _port_;
        //ServiceProvider svc_prov_;
        //RTC::CorbaPort svc_port_;
};


extern "C"
{
    DLL_EXPORT void rtc_init(RTC::Manager* manager);
};

#endif // RTC_H__

